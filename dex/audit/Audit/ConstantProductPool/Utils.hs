{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | Here you will find a set of utilities for helping with the off-chain code.
module Audit.ConstantProductPool.Utils where

import qualified Ledger.Address as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Scripts as Pl
import qualified Ledger.Value as Pl
import PlutusTx.Prelude (divide)

import Minswap.BatchOrder.Types
import Minswap.ConstantProductFactory.OnChain
import Minswap.ConstantProductPool.Utils
import Minswap.ConstantProductPool.Types
import Minswap.Types.Coin

import Cooked.MockChain
import Cooked.Tx.Constraints

import Audit.MinSwapScripts

import Data.Maybe (isJust)

-- * Order Results
--
-- The functions that follow were copied from
-- 'Minswap.ConstantProductPool.OnChain.validateDeposit' and friends, yet
-- they been made to return a 'TxOut' (or a refund).
-- The counterpart to 'validateDeposit' is 'computeValidDeposit' below; instead
-- of performing any checks, it returns the correct info.
--
-- It's spec is:
--
-- > let ValidDeposit v' addr ps' = computeValidDeposit v od ps pd ac i
-- >     txOut' = TxOut v addr ???
-- >   in (validateDeposit v od ps txOut' pd ac i @?= ps'
-- >   && (case catchError (validateDeposit v od ps txOut pd ac i) of
-- >       Left _  -> False
-- >       Right ps'' -> ps'' == ps' && txOut' == txOut
--
-- That is, 'validateDeposit' validates parms iff 'computeValidDeposit' returns it.
--
-- We have one of each of those functions for each of the order types supported by MinSwap

-- The result of 'computeValidDeposit' is a type that can be interpreted
-- to generate a new pool state and a set of constraints or fail
data ValidOrderResult
  = ValidOrderResult
      { vodValueOut :: Pl.Value
      , vodAddrOut :: Pl.Address
      , vodDatumHash :: Maybe Pl.DatumHash
      , vodPoolState :: PoolState
      }
  | Refund
      { rValueOut :: Pl.Value
      , rPoolState :: PoolState
      }
  deriving Show

deriving instance Show OrderRedeemer

data RefundPolicy = ForbidRefunds | AllowRefunds | IgnoreRefunds

-- | Executes the order result with a possibility to forbid refunds.
interpretOrderResult :: (MonadFail m)
                     => RefundPolicy -> SpendableOut -> OrderDatum -> ValidOrderResult
                     -> m ([Constraint], PoolState, Integer)
interpretOrderResult _ sout od ValidOrderResult {..} =
    case Pl.addressCredential vodAddrOut of
      Pl.PubKeyCredential pkh ->
        return ([ SpendsScript orderValidator ApplyOrder (sout, od)
                , paysPK pkh vodValueOut
                ],
                vodPoolState,
                odBatcherFee od)
      _ -> fail "Don't know how to deposit to script yet"
interpretOrderResult IgnoreRefunds sout od Refund {..} =
  return ([], rPoolState, 0)
interpretOrderResult ForbidRefunds sout od Refund {} =
  fail $ "Refunds are forbidden but order " ++ show od ++ " is being refunded"
interpretOrderResult AllowRefunds sout od Refund {..} =
    case Pl.addressCredential $ odSender od of
      Pl.PubKeyCredential pkh ->
        return ([ SpendsScript orderValidator ApplyOrder (sout, od)
                , paysPK pkh rValueOut
                ],
                rPoolState,
                odBatcherFee od)
      _ -> fail "Don't know how to refund to script yet"

computeValidDeposit ::
  -- | The input Value with A&B coins
  Pl.Value ->
  -- | The Datum with the sender, receiver and fee information
  OrderDatum ->
  -- | The state of the pool
  PoolState ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  Pl.AssetClass ->
  -- | Desired minimum amount of LP coin to get
  Integer ->
  ValidOrderResult
computeValidDeposit
  val
  OrderDatum
    { odOutputADA,
      odBatcherFee,
      odReceiver,
      odReceiverDatumHash,
      odSender
    }
  state@( !amountA,
          !amountB,
          !totalLiquidity,
          !liquidityShare,
          !rootKLast
          )
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
        (!deltaA', !deltaB', !deltaLiquidity) =
          case calculateDepositAmount deltaA deltaB amountA amountB totalLiquidity of
            Just da -> da
            Nothing -> error "We're computing a suposedly valid deposit; should be always Just"

        (mCoinAAmountOut, mCoinBAmountOut, adaAmountOut)
          | deltaB > deltaB' =
            if coinB == adaCoin
            then (Nothing, Nothing, deltaB - deltaB' + odOutputADA)
            else (Nothing, Just $ deltaB - deltaB', odOutputADA)
          | deltaA > deltaA' =
            if coinA == adaCoin
            then (Nothing, Nothing, deltaA - deltaA' + odOutputADA)
            else (Just $ deltaA - deltaA', Nothing, odOutputADA)
          | otherwise = (Nothing, Nothing, odOutputADA)

        newAmountA = amountA + deltaA'
        newAmountB = amountB + deltaB'
        deltaLiquidityShare =
          if feeOn
            then calculateProfitSharing rootKLast amountA amountB totalLiquidity
            else 0
        newRootKLast = if feeOn then calSqrt (newAmountA * newAmountB) else rootKLast

        lpAmountOut = Pl.assetClassValue lpCoin deltaLiquidity
        adaAmountOut' = Pl.assetClassValue adaCoin adaAmountOut
        coinAAmountOut = maybe mempty (Pl.assetClassValue coinA) mCoinAAmountOut
        coinBAmountOut = maybe mempty (Pl.assetClassValue coinB) mCoinBAmountOut
     in if deltaLiquidity < minimumLP
        then Refund
             (Pl.assetClassValue coinA deltaA
              <> Pl.assetClassValue coinB deltaB
              <> Pl.assetClassValue adaCoin odOutputADA)
             state
        else ValidOrderResult
          { vodValueOut = lpAmountOut <> adaAmountOut' <> coinAAmountOut <> coinBAmountOut
          , vodAddrOut = odReceiver
          , vodDatumHash = Nothing
          , vodPoolState = ( newAmountA,
                            newAmountB,
                            totalLiquidity + deltaLiquidity + deltaLiquidityShare,
                            liquidityShare + deltaLiquidityShare,
                            newRootKLast
                          )
          }

-- * Withdraw

-- Computes a withdraw (not a refund). That is, a value that
-- would satisfy the @validAmount && validReceiver@ branch of the
-- validateWithdraw function.
computeValidWithdraw ::
  -- | The input Value with LP coins
  Pl.Value ->
  -- | The Datum with the sender and receiver information
  OrderDatum ->
  -- | The state of the pool
  PoolState ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  Pl.AssetClass ->
  -- | Minimum amount of coin A to withdraw
  Integer ->
  -- | Minimum amount of coin B to withdraw
  Integer ->
  ValidOrderResult
computeValidWithdraw
  val
  OrderDatum
    { odOutputADA,
      odReceiver,
      odReceiverDatumHash,
      odSender
    }
  state@( !amountA,
          !amountB,
          !totalLiquidity,
          !liquidityShare,
          !rootKLast
          )
  PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdProfitSharing
    }
  lpCoin
  minimumCoinA
  minimumCoinB =
    let !feeOn = isJust pdProfitSharing
        !deltaLiquidity = Pl.assetClassValueOf val lpCoin
        !deltaA = (deltaLiquidity * amountA) `divide` totalLiquidity
        !deltaB = (deltaLiquidity * amountB) `divide` totalLiquidity

        (coinAAmountOut, coinBAmountOut, mAdaAmountOut)
          | coinA == adaCoin = ( deltaA + odOutputADA
                              , deltaB
                              , Nothing )
          | coinB == adaCoin = ( deltaA
                              , deltaB + odOutputADA
                              , Nothing )
          | otherwise = (deltaA , deltaB , Just odOutputADA)

        newAmountA = amountA - deltaA
        newAmountB = amountB - deltaB
        deltaLiquidityShare =
          if feeOn
            then calculateProfitSharing rootKLast amountA amountB totalLiquidity
            else 0
        newRootKLast = if feeOn then calSqrt (newAmountA * newAmountB) else rootKLast
        poolHasSufficientABCoins = deltaA >= minimumCoinA && deltaB >= minimumCoinB
     in if not poolHasSufficientABCoins
        then Refund
               (Pl.assetClassValue lpCoin deltaLiquidity
               <> Pl.assetClassValue adaCoin odOutputADA)
               state
        else ValidOrderResult
          (Pl.assetClassValue coinA coinAAmountOut
           <> Pl.assetClassValue coinB coinBAmountOut
           <> maybe mempty (Pl.assetClassValue adaCoin) mAdaAmountOut)
          odReceiver
          odReceiverDatumHash
          ( newAmountA,
            newAmountB,
            totalLiquidity - deltaLiquidity + deltaLiquidityShare,
            liquidityShare + deltaLiquidityShare,
            newRootKLast
          )

-- * OneSided Deposit

computeValidOneSideDeposit ::
  -- | The input Value with the coin to offer
  Pl.Value ->
  -- | The Datum with the sender and receiver information
  OrderDatum ->
  -- | The state of the pool
  PoolState ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  Pl.AssetClass ->
  -- | Desired coin to swap & deposit
  Pl.AssetClass ->
  -- | Minimum amount of LP token
  Integer ->
  ValidOrderResult
computeValidOneSideDeposit
  val
  OrderDatum
    { odOutputADA,
      odBatcherFee,
      odReceiver,
      odReceiverDatumHash,
      odSender
    }
  state@( !amountA,
          !amountB,
          !totalLiquidity,
          !liquidityShare,
          !rootKLast
          )
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

        lpAmountOut = deltaLiquidity
        adaAmountOut = odOutputADA

        (newAmountA, newAmountB) =
          if coinA /= desiredCoin
            then (amountA + deltaIn, amountB)
            else (amountA, amountB + deltaIn)
        deltaLiquidityShare =
          if feeOn
            then calculateProfitSharing rootKLast amountA amountB totalLiquidity
            else 0
        newRootKLast = if feeOn then calSqrt (newAmountA * newAmountB) else rootKLast
     in if deltaLiquidity < minimumLP
        then Refund
               (Pl.assetClassValue coinIn deltaIn
                <> Pl.assetClassValue adaCoin odOutputADA)
               state
        else ValidOrderResult
          (Pl.assetClassValue adaCoin adaAmountOut
           <> Pl.assetClassValue lpCoin lpAmountOut)
          odReceiver
          odReceiverDatumHash
          ( newAmountA,
            newAmountB,
            totalLiquidity + deltaLiquidity + deltaLiquidityShare,
            liquidityShare + deltaLiquidityShare,
            newRootKLast
          )

-- * Swap Exact Out

computeValidSwapExactOut ::
  -- | The input Value with the coin to offer
  Pl.Value ->
  -- | The Datum with the sender, receiver and fee information
  OrderDatum ->
  -- | The state of the pool
  PoolState ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Desired coin to buy
  Pl.AssetClass ->
  -- | Expected amount of buy coin to get
  Integer ->
  ValidOrderResult
computeValidSwapExactOut
  val
  OrderDatum
    { odOutputADA,
      odBatcherFee,
      odReceiver,
      odReceiverDatumHash,
      odSender
    }
  state@( !amountA,
          !amountB,
          !totalLiquidity,
          !liquidityShare,
          !rootKLast
          )
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
          Nothing -> error "Your params to SwapExactOut didn't enable me to compute a valid getAmountIn"
        -- desiredCoinAmountOut = assetClassValueOf txOutVal desiredCoin
        -- coinInAmountOut = assetClassValueOf txOutVal coinIn
        -- adaAmountOut = assetClassValueOf txOutVal adaCoin
        (desiredCoinAmountOut, coinInAmountOut, mAdaAmountOut)
          | desiredCoin == adaCoin =
            ( expectedReceive + odOutputADA
            , maximumDeltaIn - deltaIn
            , Nothing
            )
          | coinIn == adaCoin =
            ( expectedReceive
            , maximumDeltaIn - deltaIn + odOutputADA
            , Nothing
            )
          | otherwise =
            ( expectedReceive
            , maximumDeltaIn - deltaIn
            , Just odOutputADA
            )
        newAmountA = if coinA /= desiredCoin then amountA + deltaIn else amountA - expectedReceive
        newAmountB = if coinA /= desiredCoin then amountB - expectedReceive else amountB + deltaIn
     in if deltaIn > maximumDeltaIn
        then Refund
               (Pl.assetClassValue coinIn maximumDeltaIn
               <> Pl.assetClassValue adaCoin odOutputADA)
               state
        else ValidOrderResult
          (Pl.assetClassValue desiredCoin desiredCoinAmountOut
           <> Pl.assetClassValue coinIn coinInAmountOut
           <> maybe mempty (Pl.assetClassValue adaCoin) mAdaAmountOut)
          odReceiver
          odReceiverDatumHash
          ( newAmountA
          , newAmountB
          , totalLiquidity
          , liquidityShare
          , rootKLast
          )

-- * Swap Exact In

computeValidSwapExactIn ::
  -- | The input Value with the coin to offer
  Pl.Value ->
  -- | The Datum with the sender, receiver and fee information
  OrderDatum ->
  -- | The state of the pool
  PoolState ->
  -- | The Datum specifying which coins are in the pool and profit sharing settings
  PoolDatum ->
  -- | Desired coin to buy
  Pl.AssetClass ->
  -- | Minimum amount of buy coin to get
  Integer ->
  ValidOrderResult
computeValidSwapExactIn
  val
  OrderDatum
    { odOutputADA,
      odBatcherFee,
      odReceiver,
      odReceiverDatumHash,
      odSender
    }
  state@( !amountA,
          !amountB,
          !totalLiquidity,
          !liquidityShare,
          !rootKLast
          )
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
        -- desiredCoinAmountOut = assetClassValueOf txOutVal desiredCoin
        (desiredCoinAmountOut, mAdaAmountOut)
          | desiredCoin == adaCoin = (deltaOut + odOutputADA, Nothing)
          | otherwise = (deltaOut, Just odOutputADA)

        newAmountA = if coinA /= desiredCoin then amountA + deltaIn else amountA - deltaOut
        newAmountB = if coinA /= desiredCoin then amountB - deltaOut else amountB + deltaIn
     in if deltaOut < minimumReceive
        then Refund
                (Pl.assetClassValue adaCoin odOutputADA
                 <> Pl.assetClassValue coinIn deltaIn)
               state
        else ValidOrderResult
               (Pl.assetClassValue desiredCoin desiredCoinAmountOut
                <> maybe mempty (Pl.assetClassValue adaCoin) mAdaAmountOut)
               odReceiver
               odReceiverDatumHash
               ( newAmountA
               , newAmountB
               , totalLiquidity
               , liquidityShare
               , rootKLast
               )

-- * PoolDatum and PoolState
--
-- The functions in "Minswap.ConstantProductPool.OnChain" use this 5-tuple
-- to represent the pool state; we create a synonym and make its relationship
-- with 'PoolDatum' explicit.

type PoolState = (Integer, Integer, Integer, Integer, Integer)

poolDatumToState :: (SpendableOut, PoolDatum) -> Pl.AssetClass -> PoolState
poolDatumToState (sout, PoolDatum{..}) liqAC =
  let coinA = Pl.assetClassValueOf (sOutValue sout) pdCoinA
      coinB = Pl.assetClassValueOf (sOutValue sout) pdCoinB
      liqShare = Pl.assetClassValueOf (sOutValue sout) liqAC
   in (coinA, coinB, pdTotalLiquidity, liqShare, pdRootKLast)

updatePoolDatum :: PoolState -> Pl.AssetClass -> PoolDatum -> (PoolDatum, Pl.Value)
updatePoolDatum ps poolNFTClass PoolDatum{..} =
   (PoolDatum pdCoinA pdCoinB (psTotalLiq ps) (psRootKLast ps) pdProfitSharing
   , Pl.assetClassValue pdCoinA (psAmountA ps)
    <> Pl.assetClassValue pdCoinB (psAmountB ps)
    <> Pl.assetClassValue poolNFTClass 1
    <> Pl.assetClassValue mkFactoryCoin 1)

psAmountA :: PoolState -> Integer
psAmountA (x, _, _, _, _) = x

psAmountB :: PoolState -> Integer
psAmountB (_, x, _, _, _) = x

psTotalLiq :: PoolState -> Integer
psTotalLiq (_, _, x, _, _) = x

psLiquidityShare :: PoolState -> Integer
psLiquidityShare (_, _, _, x, _) = x

psRootKLast :: PoolState -> Integer
psRootKLast (_, _, _, _, x) = x
