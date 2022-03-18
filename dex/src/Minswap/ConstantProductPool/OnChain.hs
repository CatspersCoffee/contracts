{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module Minswap.ConstantProductPool.OnChain
  ( mkPoolScript,
    mkLicenseSymbol,
    mkOwnerTokenName,
  )
where

import Data.Maybe (fromJust)
import Ledger
  ( CurrencySymbol,
    POSIXTime (POSIXTime),
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoOutRef, txInInfoResolved),
    TxInfo
      ( txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories,
        txInfoValidRange
      ),
    TxOut (..),
    Value,
    addressCredential,
    after,
    findOwnInput,
    toPubKeyHash,
  )
import Minswap.BatchOrder.Types
  ( OrderDatum (..),
    OrderStep (Deposit, OneSideDeposit, SwapExactIn, SwapExactOut, Withdraw),
  )
import Minswap.ConstantProductFactory.OnChain (mkFactoryCoin)
import Minswap.ConstantProductLiquidity.OnChain
  ( mkLiquiditySymbol,
  )
import Minswap.ConstantProductPool.Types
  ( PoolDatum
      ( PoolDatum,
        pdCoinA,
        pdCoinB,
        pdProfitSharing,
        pdRootKLast,
        pdTotalLiquidity
      ),
    PoolParams (..),
    PoolRedeemer (ApplyPool, UpdateFeeTo, WithdrawLiquidityShare),
    psFeeTo,
    psFeeToDatumHash,
  )
import Minswap.ConstantProductPool.Utils
  ( calSqrt,
    calculateDepositAmount,
    calculateProfitSharing,
    calculateSwapAmount,
    getAmountIn,
    getAmountOut,
  )
import Minswap.ConstantProductPoolNFT.OnChain (mkNFTSymbol)
import Minswap.ConstantProductPoolNFT.Utils
  ( poolNFTOf,
  )
import Minswap.Types.Coin
  ( adaCoin,
    assetClass,
    assetClassValueOf,
    tokenNameOf,
  )
import Minswap.Utils.OnChainUtils
  ( assertPoolValue,
    bsToInteger,
    mustFindScriptDatum,
  )
import qualified Plutonomy
import Plutus.V1.Ledger.Api (Address, Script, unValidatorScript)
import Plutus.V1.Ledger.Value (AssetClass, TokenName (TokenName), Value (Value), assetClassValue, currencySymbol, tokenName)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude
  ( Bool (False, True),
    BuiltinData,
    Integer,
    Maybe (Just, Nothing),
    divide,
    elem,
    error,
    isJust,
    mempty,
    negate,
    otherwise,
    (!!),
    ($),
    (&&),
    (*),
    (+),
    (-),
    (/=),
    (<=),
    (<>),
    (==),
    (>),
    (>=),
    (||),
  )
import Text.Hex (Text, decodeHex)

mkPoolScript :: Script
mkPoolScript =
  unValidatorScript $
    Plutonomy.optimizeUPLC $
      Plutonomy.validatorToPlutus originalPoolScript

originalPoolScript :: Plutonomy.Validator
originalPoolScript =
  Plutonomy.mkValidatorScript
    ( $$(PlutusTx.compile [||mkPoolValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode poolParams
    )
  where
    !poolParams =
      PoolParams
        { ppNftSymbol = mkNFTSymbol,
          ppLiquiditySymbol = mkLiquiditySymbol,
          ppFactoryCoin = mkFactoryCoin,
          ppLicenseSymbol = mkLicenseSymbol,
          ppOwnerTokenName = mkOwnerTokenName
        }

licenseSymbolHardCode :: Text.Hex.Text
licenseSymbolHardCode = "2f2e0404310c106e2a260e8eb5a7e43f00cff42c667489d30e179816"

{-# INLINEABLE mkOwnerTokenName #-}
mkOwnerTokenName :: TokenName
mkOwnerTokenName = tokenName $ fromJust $ decodeHex "4f574e4552"

{-# INLINEABLE mkLicenseSymbol #-}
mkLicenseSymbol :: CurrencySymbol
mkLicenseSymbol = currencySymbol $ fromJust $ decodeHex licenseSymbolHardCode

{-# INLINEABLE mkPoolValidator #-}
mkPoolValidator ::
  PoolParams ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkPoolValidator pp rawDatum rawRedeemer rawContext =
  if case redeemer of
    ApplyPool batcherAddress licenseIndex ->
      validateApplyPool pp datum ctx batcherAddress licenseIndex
    UpdateFeeTo ownerIndex ->
      validateUpdateFeeTo pp datum ctx ownerIndex
    WithdrawLiquidityShare ownerIndex feeToIndex ->
      validateWithdrawLiquidityShare pp datum ctx ownerIndex feeToIndex
    then ()
    else error ()
  where
    datum = PlutusTx.unsafeFromBuiltinData @PoolDatum rawDatum
    redeemer = PlutusTx.unsafeFromBuiltinData @PoolRedeemer rawRedeemer
    ctx = PlutusTx.unsafeFromBuiltinData @ScriptContext rawContext

-- | The 'validateApplyPool' function validates the orders will be correctly applied to the pool
--   by valid batcher
--
-- 1. Validate that the pool value must contains necessary token inside
-- 2. A valid batcher must sign the transaction
-- 3. The license of batcher is not expired
-- 4. Pool's NFT token must be the same between input and output
-- 5. Factory NFT token in input have quantity = 1
-- 6. Factory NFT token in output have quantity = 1
-- 7. The delta liquidity after applying through all orders must be the same
--    with minted amount of LP token
-- 8. Validate that pool value in the output must be the same with the calculated amount
--    after applying through all orders
validateApplyPool ::
  -- | Params of PoolValidator
  PoolParams ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | ScriptContext
  ScriptContext ->
  -- | Address of valid batcher which will submit this batch transaction
  Address ->
  -- | Index of license's UTxO in the transaction inputs
  Integer ->
  Bool
validateApplyPool
  PoolParams
    { ppNftSymbol,
      ppLiquiditySymbol,
      ppLicenseSymbol,
      ppFactoryCoin
    }
  datum@PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdRootKLast = rootKLastIn,
      pdTotalLiquidity = totalLiquidityIn
    }
  ctx
  batcherAddress
  licenseIndex =
    let info :: TxInfo
        info = scriptContextTxInfo ctx

        signatories :: [PubKeyHash]
        signatories = txInfoSignatories info

        txInputs :: [TxInInfo]
        !txInputs = txInfoInputs info

        txOutputs :: [TxOut]
        !txOutputs = txInfoOutputs info

        ownInput :: TxInInfo
        !ownInput = case findOwnInput ctx of
          Just txInInfo -> txInInfo
          _ -> error ()

        !ownAddress = txOutAddress $ txInInfoResolved ownInput

        ownOutput :: TxOut
        !ownOutput = case [o | o <- txOutputs, ownAddress == txOutAddress o] of
          [o] -> o
          _ -> error ()

        outputPoolDatum = mustFindScriptDatum @PoolDatum ownOutput info
        rootKLastOut = pdRootKLast outputPoolDatum

        !inVal = txOutValue $ txInInfoResolved ownInput
        !outVal = txOutValue ownOutput

        mintValue = txInfoMint info
        validTimeRange = txInfoValidRange info
        validPoolValue = assertPoolValue coinA coinB lpCoin outVal
        nftCoinInInput = poolNFTOf inVal ppNftSymbol
        nftCoinInOutput = poolNFTOf outVal ppNftSymbol
        lpCoin = assetClass ppLiquiditySymbol (tokenNameOf nftCoinInInput)
        totalLiquidityOut = pdTotalLiquidity outputPoolDatum
        factoryCoinIn = assetClassValueOf inVal ppFactoryCoin
        factoryCoinOut = assetClassValueOf outVal ppFactoryCoin
        amountAIn = assetClassValueOf inVal coinA
        amountBIn = assetClassValueOf inVal coinB
        amountAOut = assetClassValueOf outVal coinA
        amountBOut = assetClassValueOf outVal coinB
        liquidityShareIn = assetClassValueOf inVal lpCoin
        liquidityShareOut = assetClassValueOf outVal lpCoin
        totalDeltaLiquidity = totalLiquidityOut - totalLiquidityIn

        applySuccess =
          applyOrders
            userInputs
            userOutputs
            (amountAIn, amountBIn, totalLiquidityIn, liquidityShareIn, rootKLastIn)

        batcherPubKeyHash = case toPubKeyHash batcherAddress of
          Just pkh -> pkh
          Nothing -> error ()

        licenseInput :: TxInInfo
        licenseInput = txInputs !! licenseIndex

        -- Each batcher will have a license token where tts TokenName is the deadline
        -- After deadline, the batcher cannot submit a batch transaction
        licenseDeadline :: Integer
        licenseDeadline =
          let (Value mp) = txOutValue $ txInInfoResolved licenseInput
              (TokenName license) = case Map.lookup ppLicenseSymbol mp of
                Just i -> case Map.toList i of
                  [(tn, _)] -> tn
                  _ -> error ()
                _ -> error ()
           in bsToInteger license

        userInputs :: [TxInInfo]
        userInputs = [i | i <- txInputs, isUserInput i]

        userOutputs :: [TxOut]
        userOutputs = [o | o <- txOutputs, isUserOutput o]

        isUserInput :: TxInInfo -> Bool
        isUserInput txIn =
          txInInfoOutRef txIn /= txInInfoOutRef ownInput
            && txOutAddress (txInInfoResolved txIn) /= batcherAddress

        isUserOutput :: TxOut -> Bool
        isUserOutput txOut =
          let addr = txOutAddress txOut
           in txOutAddress txOut /= txOutAddress ownOutput && addr /= batcherAddress

        applyStep' = \iv od st o -> applyStep iv od st o datum lpCoin

        applyOrders ::
          [TxInInfo] ->
          [TxOut] ->
          (Integer, Integer, Integer, Integer, Integer) ->
          Bool
        applyOrders [] [] (i1, i2, i3, i4, i5) =
          i1 == amountAOut
            && i2 == amountBOut
            && i3 == totalLiquidityOut
            && i4 == liquidityShareOut
            && i5 == rootKLastOut
        applyOrders [] _ _ = error ()
        applyOrders _ [] _ = error ()
        applyOrders (i : ins) (o : outs) state =
          let txOut = txInInfoResolved i
              inputValue = txOutValue txOut
              orderDatum = mustFindScriptDatum txOut info
              newState = applyStep' inputValue orderDatum state o
           in applyOrders ins outs newState
     in validPoolValue -- 1.
          && batcherPubKeyHash `elem` signatories -- 2.
          && after (POSIXTime licenseDeadline) validTimeRange -- 3.
          && nftCoinInInput == nftCoinInOutput -- 4.
          && factoryCoinIn == 1 -- 5.
          && factoryCoinOut == 1 -- 6.
          && assetClassValueOf mintValue lpCoin == totalDeltaLiquidity -- 7.
          && applySuccess -- 8.

-- | The 'validateUpdateFeeTo' function validates the profit sharing information in the pool datum
--   has been updated by correct Owner
--
-- 1. Pool value must be the same between input and output
-- 2. Pool's NFT token must be the same between input and output
-- 3. Validate that only Owner can perform the transaction
-- 4. Validate that if datum changes, RootKLastOut must be reset after enabling/disabling Profit Sharing feature
--    If datum doesn't change, it is updating Script Stake's credential case
-- 5. Validate that no asset will be minted in this action
validateUpdateFeeTo ::
  -- | Params of PoolValidator
  PoolParams ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | ScriptContext
  ScriptContext ->
  -- | Index of owner's UTxO in the transaction inputs
  Integer ->
  Bool
validateUpdateFeeTo
  PoolParams {ppNftSymbol, ppLicenseSymbol, ppOwnerTokenName}
  datum
  ctx
  ownerIndex =
    let info :: TxInfo
        info = scriptContextTxInfo ctx

        txInputs :: [TxInInfo]
        txInputs = txInfoInputs info

        txOutputs :: [TxOut]
        txOutputs = txInfoOutputs info

        ownInput :: TxInInfo
        ownInput = case findOwnInput ctx of
          Just txInInfo -> txInInfo
          _ -> error ()

        ownAddress = txOutAddress $ txInInfoResolved ownInput

        ownOutput :: TxOut
        ownOutput = case [ o
                           | o <- txOutputs,
                             addressCredential ownAddress == addressCredential (txOutAddress o)
                         ] of
          [o] -> o
          _ -> error ()

        outputPoolDatum = mustFindScriptDatum @PoolDatum ownOutput info
        rootKLastOut = pdRootKLast outputPoolDatum

        inVal = txOutValue $ txInInfoResolved ownInput
        outVal = txOutValue ownOutput

        nftCoinInInput = poolNFTOf inVal ppNftSymbol
        nftCoinInOutput = poolNFTOf outVal ppNftSymbol

        ownerInput :: TxInInfo
        ownerInput = txInputs !! ownerIndex

        hasOwner =
          let (Value mp) = txOutValue $ txInInfoResolved ownerInput
              ownerTokenName = case Map.lookup ppLicenseSymbol mp of
                Just i -> case Map.toList i of
                  [(tn, _)] -> tn
                  _ -> error ()
                _ -> error ()
           in ownerTokenName == ppOwnerTokenName

        validateDatum :: Bool
        validateDatum =
          if datum == outputPoolDatum
            then True
            else rootKLastOut == 0
     in inVal == outVal -- 1.
          && nftCoinInInput == nftCoinInOutput -- 2.
          && hasOwner -- 3.
          && validateDatum -- 4.
          && txInfoMint info == mempty -- 5.

-- | The 'validateWithdrawLiquidityShare' function validates the profit sharing value will be withdrawed
--   to the correct FeeTo address
--
-- 1. Pool datum must be the same between input and output
-- 2. Pool's NFT token must be the same between input and output
-- 3. Validate that profit sharing value must be withdrawed to the correct FeeTo address
-- 4. Validate that Pool Value in output = Pool Value in input - amount of liquidity share
-- 5. Validate that only Owner can perform the transaction
-- 6. Validate that no asset will be minted in this action
validateWithdrawLiquidityShare ::
  -- | Params of PoolValidator
  PoolParams ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | ScriptContext
  ScriptContext ->
  -- | Index of owner's UTxO in the transaction inputs
  Integer ->
  -- | Index to FeeTo's UTxO in the transaction outputs
  Integer ->
  Bool
validateWithdrawLiquidityShare
  PoolParams {ppLiquiditySymbol, ppNftSymbol, ppLicenseSymbol, ppOwnerTokenName}
  datum
  ctx
  ownerIndex
  feeToIndex =
    case pdProfitSharing datum of
      Nothing -> False
      Just ps ->
        let info :: TxInfo
            info = scriptContextTxInfo ctx

            txInputs :: [TxInInfo]
            txInputs = txInfoInputs info

            txOutputs :: [TxOut]
            txOutputs = txInfoOutputs info

            ownInput :: TxInInfo
            ownInput = case findOwnInput ctx of
              Just txInInfo -> txInInfo
              _ -> error ()

            ownAddress = txOutAddress $ txInInfoResolved ownInput

            ownOutput :: TxOut
            ownOutput = case [o | o <- txOutputs, ownAddress == txOutAddress o] of
              [o] -> o
              _ -> error ()

            ownerInput :: TxInInfo
            ownerInput = txInputs !! ownerIndex

            outputPoolDatum = mustFindScriptDatum @PoolDatum ownOutput info

            inVal = txOutValue $ txInInfoResolved ownInput
            outVal = txOutValue ownOutput

            nftCoinInInput = poolNFTOf inVal ppNftSymbol
            nftCoinInOutput = poolNFTOf outVal ppNftSymbol
            feeToOutput = txOutputs !! feeToIndex
            lpCoin = assetClass ppLiquiditySymbol (tokenNameOf nftCoinInInput)
            liquidityShare = assetClassValueOf inVal lpCoin
            expectedOutVal = inVal <> assetClassValue lpCoin (negate liquidityShare)
            validFeeToOutput =
              assetClassValueOf (txOutValue feeToOutput) lpCoin == liquidityShare
                && txOutAddress feeToOutput == psFeeTo ps
                && txOutDatumHash feeToOutput == psFeeToDatumHash ps
            hasOwner =
              let (Value mp) = txOutValue $ txInInfoResolved ownerInput
                  ownerTokenName = case Map.lookup ppLicenseSymbol mp of
                    Just i -> case Map.toList i of
                      [(tn, _)] -> tn
                      _ -> error ()
                    _ -> error ()
               in ownerTokenName == ppOwnerTokenName
         in datum == outputPoolDatum -- 1.
              && nftCoinInInput == nftCoinInOutput -- 2.
              && validFeeToOutput -- 3.
              && expectedOutVal == outVal -- 4.
              && hasOwner -- 5.
              && txInfoMint info == mempty -- 6.

-- | The 'applyStep' function validates the order and updates the pool state.
applyStep ::
  -- | Input value
  Value ->
  -- | Order
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer, Integer, Integer) ->
  -- | Transaction output
  TxOut ->
  -- | Information about the pool
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  AssetClass ->
  (Integer, Integer, Integer, Integer, Integer)
applyStep
  !val
  od@OrderDatum
    { odBatcherFee,
      odOutputADA,
      odStep
    }
  state
  txo
  pd
  lpCoin =
    if odBatcherFee <= 0 || odOutputADA <= 0
      then error ()
      else case odStep of
        SwapExactIn desiredCoin minimumReceive ->
          validateSwapExactIn val od state txo pd desiredCoin minimumReceive
        SwapExactOut desiredCoin expectedReceive ->
          validateSwapExactOut val od state txo pd desiredCoin expectedReceive
        Deposit minimumLP ->
          validateDeposit val od state txo pd lpCoin minimumLP
        Withdraw minimumCoinA minimumCoinB ->
          validateWithdraw val od state txo pd lpCoin minimumCoinA minimumCoinB
        OneSideDeposit desiredCoin minimumLP ->
          validateOneSideDeposit val od state txo pd lpCoin desiredCoin minimumLP

-- |
--  The 'validateSwapExactIn' function validates that the user exchanged the
--  supplied amount of offer coin for at least a minimum amount of buy coin.
validateSwapExactIn ::
  -- | The input Value with the coin to offer
  Value ->
  -- | The Datum with the sender, receiver and fee information
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer, Integer, Integer) ->
  -- | Transaction output for sending out the bought coins
  TxOut ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Desired coin to buy
  AssetClass ->
  -- | Minimum amount of buy coin to get
  Integer ->
  (Integer, Integer, Integer, Integer, Integer)
validateSwapExactIn
  val
  OrderDatum
    { odOutputADA,
      odBatcherFee,
      odReceiver,
      odReceiverDatumHash
    }
  ( !amountA,
    !amountB,
    !totalLiquidity,
    !liquidityShare,
    !rootKLast
    )
  TxOut
    { txOutValue = txOutVal,
      txOutAddress,
      txOutDatumHash
    }
  PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB
    }
  desiredCoin
  minimumReceive =
    let (!coinIn, !reserveIn, !reserveOut) =
          if coinA /= desiredCoin
            then (coinA, amountA, amountB)
            else (coinB, amountB, amountA)
        coinInAmount = assetClassValueOf val coinIn
        !deltaIn =
          if coinIn == adaCoin
            then coinInAmount - (odBatcherFee + odOutputADA)
            else coinInAmount
        !deltaOut = getAmountOut reserveIn reserveOut deltaIn
        desiredCoinAmountOut = assetClassValueOf txOutVal desiredCoin
        adaAmountOut = assetClassValueOf txOutVal adaCoin
        validAmount
          | desiredCoin == adaCoin = desiredCoinAmountOut == deltaOut + odOutputADA
          | otherwise = desiredCoinAmountOut == deltaOut && adaAmountOut == odOutputADA
        validReceiver = txOutAddress == odReceiver && txOutDatumHash == odReceiverDatumHash
     in if deltaOut >= minimumReceive && validAmount && validReceiver
          then
            if coinA /= desiredCoin
              then
                ( amountA + deltaIn,
                  amountB - deltaOut,
                  totalLiquidity,
                  liquidityShare,
                  rootKLast
                )
              else
                ( amountA - deltaOut,
                  amountB + deltaIn,
                  totalLiquidity,
                  liquidityShare,
                  rootKLast
                )
          else error ()

-- |
--  The 'validateSwapExactOut' function validates that the user exchanged the
--  the supplied amount of offer coin for exact amount of buy coin.
validateSwapExactOut ::
  -- | The input Value with the coin to offer
  Value ->
  -- | The Datum with the sender, receiver and fee information
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer, Integer, Integer) ->
  -- | Transaction output for sending out the bought coins
  TxOut ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Desired coin to buy
  AssetClass ->
  -- | Expected amount of buy coin to get
  Integer ->
  (Integer, Integer, Integer, Integer, Integer)
validateSwapExactOut
  val
  OrderDatum
    { odOutputADA,
      odBatcherFee,
      odReceiver,
      odReceiverDatumHash
    }
  ( !amountA,
    !amountB,
    !totalLiquidity,
    !liquidityShare,
    !rootKLast
    )
  TxOut
    { txOutValue = txOutVal,
      txOutAddress,
      txOutDatumHash
    }
  PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB
    }
  desiredCoin
  expectedReceive =
    let (!coinIn, !reserveIn, !reserveOut) =
          if coinA /= desiredCoin
            then (coinA, amountA, amountB)
            else (coinB, amountB, amountA)
        coinInAmount = assetClassValueOf val coinIn
        !maximumDeltaIn =
          if coinIn == adaCoin
            then coinInAmount - (odBatcherFee + odOutputADA)
            else coinInAmount
        !deltaIn = case getAmountIn reserveIn reserveOut expectedReceive of
          Just am -> am
          _ -> error ()
        desiredCoinAmountOut = assetClassValueOf txOutVal desiredCoin
        coinInAmountOut = assetClassValueOf txOutVal coinIn
        adaAmountOut = assetClassValueOf txOutVal adaCoin
        validAmount
          | desiredCoin == adaCoin =
            desiredCoinAmountOut == expectedReceive + odOutputADA
              && coinInAmountOut == maximumDeltaIn - deltaIn
          | coinIn == adaCoin =
            desiredCoinAmountOut == expectedReceive
              && coinInAmountOut == maximumDeltaIn - deltaIn + odOutputADA
          | otherwise =
            desiredCoinAmountOut == expectedReceive
              && coinInAmountOut == maximumDeltaIn - deltaIn
              && adaAmountOut == odOutputADA
        validReceiver = txOutAddress == odReceiver && txOutDatumHash == odReceiverDatumHash
     in if deltaIn <= maximumDeltaIn && validAmount && validReceiver
          then
            if coinA /= desiredCoin
              then
                ( amountA + deltaIn,
                  amountB - expectedReceive,
                  totalLiquidity,
                  liquidityShare,
                  rootKLast
                )
              else
                ( amountA - expectedReceive,
                  amountB + deltaIn,
                  totalLiquidity,
                  liquidityShare,
                  rootKLast
                )
          else error ()

-- |
--  The 'validateDeposit' function validates that the LP coins are given to the
--  liquidity provider in exchange for their A&B coins.
--
--  = Logic outline
--
--    * Calculate the LP coin amount to give the user in exchange for their
--      deposited A&B coin amounts
--
--        * If the rate deltaA / amountA != deltaB / amountB, account for the
--          change and refund the change balance to the receiver
--
--        * If there's enough LP coins in the pool to reward the deposit
--
--           * Validate whether the value of is sent to the correct
--             user (receiver)
--           * If profit sharing is enable, then we mint the amount of LP token
--             and give it back to Pool Value
--
--        * If there's NOT enough then throwing error
validateDeposit ::
  -- | The input Value with A&B coins
  Value ->
  -- | The Datum with the sender, receiver and fee information
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer, Integer, Integer) ->
  -- | Transaction output for sending out the LP coins
  TxOut ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  AssetClass ->
  -- | Desired minimum amount of LP coin to get
  Integer ->
  (Integer, Integer, Integer, Integer, Integer)
validateDeposit
  val
  OrderDatum
    { odOutputADA,
      odBatcherFee,
      odReceiver,
      odReceiverDatumHash
    }
  ( !amountA,
    !amountB,
    !totalLiquidity,
    !liquidityShare,
    !rootKLast
    )
  TxOut
    { txOutValue = txOutVal,
      txOutAddress,
      txOutDatumHash
    }
  PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdProfitSharing
    }
  lpCoin
  minimumLP =
    let !feeOn = isJust pdProfitSharing
        coinAAmount = assetClassValueOf val coinA
        coinBAmount = assetClassValueOf val coinB
        !deltaA =
          if coinA == adaCoin
            then coinAAmount - (odBatcherFee + odOutputADA)
            else coinAAmount
        !deltaB =
          if coinB == adaCoin
            then coinBAmount - (odBatcherFee + odOutputADA)
            else coinBAmount
        -- If the rate deltaA / amountA != deltaB / amountB (this case happens
        -- when having swapping actions before this depositing action because of
        -- this rate is changed), we should recalculate the real deltaA & deltaB
        -- amount and refund the change balance to the receiver (the deltaA' &
        -- deltaB')
        (!deltaA', !deltaB', !deltaLiquidity) =
          case calculateDepositAmount deltaA deltaB amountA amountB totalLiquidity of
            Just da -> da
            _ -> error ()
        lpAmountOut = assetClassValueOf txOutVal lpCoin
        adaAmountOut = assetClassValueOf txOutVal adaCoin
        coinAAmountOut = assetClassValueOf txOutVal coinA
        coinBAmountOut = assetClassValueOf txOutVal coinB
        validAmount
          | deltaB > deltaB' =
            if coinB == adaCoin
              then adaAmountOut == deltaB - deltaB' + odOutputADA && lpAmountOut == deltaLiquidity
              else
                adaAmountOut == odOutputADA
                  && lpAmountOut == deltaLiquidity
                  && deltaB - deltaB' == coinBAmountOut
          | deltaA > deltaA' =
            if coinA == adaCoin
              then adaAmountOut == deltaA - deltaA' + odOutputADA && lpAmountOut == deltaLiquidity
              else
                adaAmountOut == odOutputADA
                  && lpAmountOut == deltaLiquidity
                  && deltaA - deltaA' == coinAAmountOut
          | otherwise = lpAmountOut == deltaLiquidity && adaAmountOut == odOutputADA
        validReceiver = txOutAddress == odReceiver && txOutDatumHash == odReceiverDatumHash
        newAmountA = amountA + deltaA'
        newAmountB = amountB + deltaB'
        deltaLiquidityShare =
          if feeOn
            then calculateProfitSharing rootKLast amountA amountB totalLiquidity
            else 0
        newRootKLast = if feeOn then calSqrt (newAmountA * newAmountB) else rootKLast
     in if deltaLiquidity >= minimumLP && validAmount && validReceiver
          then
            ( newAmountA,
              newAmountB,
              totalLiquidity + deltaLiquidity + deltaLiquidityShare,
              liquidityShare + deltaLiquidityShare,
              newRootKLast
            )
          else error ()

-- |
--  The 'validateWithdraw' function validates that A&B coins are given back to
--  the liquidity provider in exchange for their LP coins.
--
--  = Logic outline
--
--    * Calculate the A&B coin amounts to withdraw based on the given LP amount
--
--        * If there's enough A&B coins in the pool to withdraw
--
--            * Validate whether the correct amounts return to the correct user (receiver)
--
--            * If profit sharing is enabled, then we mint the amount of LP and
--              give it back to Pool Value
--
--        * If there's NOT enough then throwing error
validateWithdraw ::
  -- | The input Value with LP coins
  Value ->
  -- | The Datum with the sender and receiver information
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer, Integer, Integer) ->
  -- | Transaction output for sending out the withdrawn coins
  TxOut ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  AssetClass ->
  -- | Minimum amount of coin A to withdraw
  Integer ->
  -- | Minimum amount of coin B to withdraw
  Integer ->
  (Integer, Integer, Integer, Integer, Integer)
validateWithdraw
  val
  OrderDatum
    { odOutputADA,
      odReceiver,
      odReceiverDatumHash
    }
  ( !amountA,
    !amountB,
    !totalLiquidity,
    !liquidityShare,
    !rootKLast
    )
  TxOut
    { txOutValue = txOutVal,
      txOutAddress,
      txOutDatumHash
    }
  PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdProfitSharing
    }
  lpCoin
  minimumCoinA
  minimumCoinB =
    let !feeOn = isJust pdProfitSharing
        !deltaLiquidity = assetClassValueOf val lpCoin
        !deltaA = (deltaLiquidity * amountA) `divide` totalLiquidity
        !deltaB = (deltaLiquidity * amountB) `divide` totalLiquidity
        coinAAmountOut = assetClassValueOf txOutVal coinA
        coinBAmountOut = assetClassValueOf txOutVal coinB
        adaAmountOut = assetClassValueOf txOutVal adaCoin
        validAmount
          | coinA == adaCoin =
            coinAAmountOut == deltaA + odOutputADA
              && coinBAmountOut == deltaB
          | coinB == adaCoin =
            coinBAmountOut == deltaB + odOutputADA
              && coinAAmountOut == deltaA
          | otherwise =
            coinAAmountOut == deltaA
              && coinBAmountOut == deltaB
              && adaAmountOut == odOutputADA
        validReceiver =
          txOutAddress == odReceiver
            && txOutDatumHash == odReceiverDatumHash
        newAmountA = amountA - deltaA
        newAmountB = amountB - deltaB
        deltaLiquidityShare =
          if feeOn
            then calculateProfitSharing rootKLast amountA amountB totalLiquidity
            else 0
        newRootKLast = if feeOn then calSqrt (newAmountA * newAmountB) else rootKLast
        poolHasSufficientABCoins = deltaA >= minimumCoinA && deltaB >= minimumCoinB
     in if poolHasSufficientABCoins && validAmount && validReceiver
          then
            ( newAmountA,
              newAmountB,
              totalLiquidity - deltaLiquidity + deltaLiquidityShare,
              liquidityShare + deltaLiquidityShare,
              newRootKLast
            )
          else error ()

-- |
--  The 'validateOneSideDeposit' function validates that the LP coins are given to the
--  liquidity provider in exchange for a single coin
--
--  = Logic outline
--
--    * Calculate the amount of token which will be swapped to supply enough amount for depositing (deltaSwapIn)
--    * Calculate the amount of token which will be received after swapping a part of `coinIn` (deltaDepositOut)
--    * Calculate the LP coin amount to give the user in exchange based on
--       the calculated amount above (deltaIn - deltaSwapIn, deltaDepositOut)
--
--        * If there's enough LP coins in the pool to reward the deposit
--
--           * Validate whether the value of is sent to the correct
--             user (receiver)
--           * If profit sharing is enable, then we mint the amount of LP token
--             and give it back to Pool Value
--
--        * If there's NOT enough then throwing error
validateOneSideDeposit ::
  -- | The input Value with the coin to offer
  Value ->
  -- | The Datum with the sender and receiver information
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer, Integer, Integer) ->
  -- | Transaction output for sending out the LP coins
  TxOut ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  AssetClass ->
  -- | Desired coin to swap & deposit
  AssetClass ->
  -- | Minimum amount of LP token
  Integer ->
  (Integer, Integer, Integer, Integer, Integer)
validateOneSideDeposit
  val
  OrderDatum
    { odOutputADA,
      odBatcherFee,
      odReceiver,
      odReceiverDatumHash
    }
  ( !amountA,
    !amountB,
    !totalLiquidity,
    !liquidityShare,
    !rootKLast
    )
  TxOut
    { txOutValue = txOutVal,
      txOutAddress,
      txOutDatumHash
    }
  PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdProfitSharing
    }
  lpCoin
  desiredCoin
  minimumLP =
    let !feeOn = isJust pdProfitSharing
        (!coinIn, !reserveIn, !reserveOut) =
          if coinA /= desiredCoin
            then (coinA, amountA, amountB)
            else (coinB, amountB, amountA)
        coinInAmount = assetClassValueOf val coinIn
        !deltaIn =
          if coinIn == adaCoin
            then coinInAmount - (odBatcherFee + odOutputADA)
            else coinInAmount
        !deltaSwapIn = calculateSwapAmount reserveIn deltaIn

        !deltaDepositOut = getAmountOut reserveIn reserveOut deltaSwapIn

        !deltaLiquidity = (deltaDepositOut * totalLiquidity) `divide` (reserveOut - deltaDepositOut)

        lpAmountOut = assetClassValueOf txOutVal lpCoin
        adaAmountOut = assetClassValueOf txOutVal adaCoin
        validAmount = lpAmountOut == deltaLiquidity && adaAmountOut == odOutputADA
        validReceiver =
          txOutAddress == odReceiver
            && txOutDatumHash == odReceiverDatumHash

        (newAmountA, newAmountB) =
          if coinA /= desiredCoin
            then (amountA + deltaIn, amountB)
            else (amountA, amountB + deltaIn)
        deltaLiquidityShare =
          if feeOn
            then calculateProfitSharing rootKLast amountA amountB totalLiquidity
            else 0
        newRootKLast = if feeOn then calSqrt (newAmountA * newAmountB) else rootKLast
     in if deltaLiquidity >= minimumLP && validAmount && validReceiver
          then
            ( newAmountA,
              newAmountB,
              totalLiquidity + deltaLiquidity + deltaLiquidityShare,
              liquidityShare + deltaLiquidityShare,
              newRootKLast
            )
          else error ()
