{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | This is the off-chain code for creating and submitting transactions
-- targeted at the MinSwap scripts. This entire off-chain API assumes the
-- existence of a valid license belonging to wallet 9 (the batcher) and
-- that wallet 8 is identified as the "owner"; to actually use this in a more general
-- setting a few small changes would be required.
module Audit.OffChain where

import Control.Monad
import Data.Default
import Data.Foldable
import Data.List (findIndex, sort)
import Data.Maybe (fromJust)
import qualified Data.Set as S

import qualified Ledger as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Value as Pl
import qualified Ledger.Ada as Ada
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl

import Minswap.BatchOrder.Types
import Minswap.ConstantProductFactory.OnChain
import Minswap.ConstantProductPool.Utils
import Minswap.ConstantProductPool.OnChain
import Minswap.ConstantProductPool.Types
import Minswap.ConstantProductPoolNFT.OnChain
import Minswap.ConstantProductLiquidity.OnChain

import Cooked.MockChain
import Cooked.Tx.Constraints

import Minswap.Types.Coin

import Audit.ConstantProductPool.Utils
import Audit.MinSwapScripts

import qualified Ledger.Typed.Scripts as Scripts

minimumAdaPerUTxO :: Integer
minimumAdaPerUTxO = 2_000_000

-- * Liquidity Pools

-- ** Creating a Liquidity Pool
--
-- Creates a liquidity pool with two different assets. The setup is tricky given that
-- there are three different tokens that must be managed:

data TxCreatePool = TxCreatePool
  { tcpCoinA :: (Pl.AssetClass, Integer)
  , tcpCoinB :: (Pl.AssetClass, Integer)
  } deriving Show


initialPoolDatum :: TxCreatePool -> PoolDatum
initialPoolDatum p0 =
  let TxCreatePool{..} = sortTxCreatePool p0
      liqAmount = calculateInitialLiquidity (snd tcpCoinA) (snd tcpCoinB)
   in PoolDatum
        { pdCoinA = fst tcpCoinA
        , pdCoinB = fst tcpCoinB
        , pdTotalLiquidity = liqAmount
        , pdRootKLast = 0
        , pdProfitSharing = Nothing
        }

sortTxCreatePool :: TxCreatePool -> TxCreatePool
sortTxCreatePool (TxCreatePool a@(ax, _) b@(bx, _))
  | ax < bx = TxCreatePool a b
  | otherwise = TxCreatePool b a

-- Creates a pool and returns the NFT, as well as the TxId of the pool UTxO.
txCreatePool :: (MonadBlockChain m) => TxCreatePool -> m (Pl.AssetClass, Pl.TxId)
txCreatePool p0 = do
  let parm@TxCreatePool{..} = sortTxCreatePool p0
  let oneFactoryToken = Pl.assetClassValue mkFactoryCoin 1
  let valA = uncurry Pl.assetClassValue tcpCoinA
  let valB = uncurry Pl.assetClassValue tcpCoinB

  -- /ASSUMPTION: user has coinA and coinB in the same UTxO/
  -- Find a single user UTxO that contains coinA and coinB to be sent to the pool
  pkh <- ownPaymentPubKeyHash
  out : _ <- pkUtxosSuchThatValue pkh (hasAssetClasses $ map fst [tcpCoinA, tcpCoinB])

  -- Use the utxo one for the nftTokenName
  let nftAC = Pl.assetClass mkNFTSymbol (mkNFTTokenName $ fst out)
  let nft = Pl.assetClassValue nftAC 1

  -- Calculate the amount of liquidity based on MinSwaps own function
  let liqAmount = calculateInitialLiquidity (snd tcpCoinA) (snd tcpCoinB)
  let liqAC = Pl.assetClass mkLiquiditySymbol (mkNFTTokenName $ fst out)
  -- Must burn the first 'minimumLiquidity' tokens
  let liq = Pl.assetClassValue liqAC (liqAmount - minimumLiquidity)

  poolTxId <- validateTxSkel $ TxSkel (Just parm)
    (def { adjustUnbalTx = True })
    [ PaysScript poolValidator
        [( initialPoolDatum p0
         , oneFactoryToken
           <> nft
           <> valA
           <> valB
         )]
    , mints [factoryMP] oneFactoryToken
    , Mints (Just $ fst out) [nftMP] nft
    , mints [lpMP] liq
    -- TODO: make some hand balancing here by returning leftover coinA, coinB and ada to the pk
    , paysPK pkh liq
    -- VCM: funny enough, uncommenting this breaks things and it seems related to our balancing mechanism.
    -- ,SpendsPK out , PaysPK pkh (Ada.lovelaceValueOf $ lovInOut - liq)
    ]
  return (nftAC, poolTxId)

type WithProfitSharing = Bool

-- | Creates a pool, pottentially turning on profit-sharing and attempts to create and execute an order.
-- This was Copied of 'BOP.createAndExecute' and modified slightly.
createAndExecute :: (MonadMockChain m)
                 => TxCreatePool
                 -> WithProfitSharing
                 -> [Pl.AssetClass -> (Order, Wallet)]
                 -> m Pl.AssetClass
createAndExecute pool ps orderGens = fst <$> createAndExecute' pool ps orderGens

-- Auxiliar function that returns an order type in case we want to tabulate it.
createAndExecute' :: (MonadMockChain m)
                 => TxCreatePool
                 -> WithProfitSharing
                 -> [Pl.AssetClass -> (Order, Wallet)]
                 -> m (Pl.AssetClass, [OrderType])
createAndExecute' pool ps orderGens = do
  (nftAC, _) <- txCreatePool pool `as` wallet 1
  when ps $ void $ txUpdateFeeTo (feeUpd8 nftAC) `as` wMinSwap
  let liqAC = Pl.assetClass mkLiquiditySymbol (snd $ Pl.unAssetClass nftAC)
  os <- forM orderGens $ \gen
           -> let (order, orderW) = gen liqAC
              in (orderType order,) <$> txCreateOrder order `as` orderW
  let (otys, oids) = unzip os
  unless (null oids) $ void $ txApplyOrders (TxApplyOrder nftAC oids) `as` wBatcher
  pure (nftAC, otys)
 where
   feeUpd8 ac = TxUpdateFeeTo ac (Just $ ProfitSharing (walletAddress wMinSwap) Nothing)

-- Here we need an "owner"; that is, the minswap administrators that start
-- off with a 'mkOwnerTokenName' token.
wMinSwap :: Wallet
wMinSwap = wallet 8

wBatcher :: Wallet
wBatcher = wallet 9

createAndExecuteSingle :: (MonadMockChain m) => TxCreatePool -> (Pl.AssetClass -> Order) -> Wallet -> m ()
createAndExecuteSingle pool order orderW = void $ createAndExecute pool False [\ac -> (order ac, orderW)]

-- * Undefined transactions

data TxUpdateFeeTo = TxUpdateFeeTo
  { uftPoolNFTClass :: Pl.AssetClass
  , uftProfitSharing :: Maybe ProfitSharing
  } deriving Show

-- Make sure that this transaction is sent from the "owner"s address: i.e.: someone
-- that posseses a token named "MINSWAP" belonging to the licenseSymbol.
txUpdateFeeTo :: (MonadBlockChain m) => TxUpdateFeeTo -> m Pl.TxId
txUpdateFeeTo parm@TxUpdateFeeTo{..} = do
  pkh <- ownPaymentPubKeyHash
  ownerOut : _ <- pkUtxosSuchThatValue pkh (hasAssetClass (Pl.assetClass mkLicenseSymbol mkOwnerTokenName))

  -- find the pool in question
  [(poolOut, poolDat)] <- scriptUtxosSuchThat poolValidator (\_ -> hasAssetClass uftPoolNFTClass)
  let poolDat' = poolDat
        { pdRootKLast = 0
        , pdProfitSharing = uftProfitSharing
        }

  let constr =
        [ SpendsScript poolValidator (UpdateFeeTo 0) (poolOut, poolDat)
        , PaysScript poolValidator [(poolDat', sOutValue poolOut)]
        , SpendsPK ownerOut
        , paysPK pkh (sOutValue ownerOut Pl.- adaFee)
        ]
  validateTxSkel $ TxSkel (Just parm)
    (def { unsafeModTx = RawModTx $ alterRedeemerAt (fst poolOut) $ computeRedeemerFor (fst ownerOut) })
    constr
  where
   adaFee = Pl.assetClassValue adaCoin 10

   computeRedeemerFor :: Pl.TxOutRef -> [Pl.TxIn] -> outs -> PoolRedeemer
   computeRedeemerFor ownerRef txIns _
     = let ownerIx = fromJust $ findIndex ((== ownerRef) . Pl.txInRef) txIns
        in UpdateFeeTo (fromIntegral ownerIx)

newtype TxWithdrawLiquidityShare = TxWithdrawLiquidityShare
  { -- | The NFT identifying the pool.
    wlsPoolNFTClass :: Pl.AssetClass
  }
  deriving (Show)

txWithdrawLiquidityShare :: (MonadBlockChain m) => TxWithdrawLiquidityShare -> m Pl.TxId
txWithdrawLiquidityShare = txWithdrawLiquidityShare' poolValidator

-- This auxiliar version receives a potentially different target poolValidator-script,
-- enabling us to execute our datum hijacking whilst reusing the template of
-- a working transaction
txWithdrawLiquidityShare' :: (MonadBlockChain m)
                          => Scripts.TypedValidator PoolValidator
                          -> TxWithdrawLiquidityShare -> m Pl.TxId
txWithdrawLiquidityShare' tgt parm@TxWithdrawLiquidityShare{..} = do
  pkh <- ownPaymentPubKeyHash
  senderOuts <- pkUtxosSuchThatValue pkh (\_ -> True)
  -- We'll let anyone create a txWithdrawLiquidityShare transaction, but we will prefer using an input
  -- that ontains the owner token.
  let senderOut = case filter (hasAssetClass (Pl.assetClass mkLicenseSymbol mkOwnerTokenName) . sOutValue) senderOuts of
        (r:_) -> r
        [] -> head senderOuts
  let ownerRef = fst senderOut

  [poolUtxo@(poolOut, poolDat)] <- scriptUtxosSuchThat poolValidator (\_ -> hasAssetClass wlsPoolNFTClass)

  let liquidityShare = assetClassValueOf (sOutValue poolOut) liqAC
  let liquidityShareVal = assetClassValue liqAC liquidityShare

  -- Withdrawing the liquidity share only works when profit-sharing is enabled in the first place;
  -- hence, pdProfitSharing must be a 'Just'; we make an additional simplifying assumption that
  -- the address registered to receive the fees is a PublicKey address.
  let Just (ProfitSharing psFeeTo@(Pl.Address (Pl.PubKeyCredential psFeeToPKH) _) _)
        = pdProfitSharing poolDat


  let constr =
        [ SpendsScript poolValidator (WithdrawLiquidityShare 0 0) poolUtxo
        , PaysScript tgt [(poolDat, sOutValue poolOut <> Pl.negate liquidityShareVal)]
        , SpendsPK senderOut
        , paysPK pkh (sOutValue senderOut Pl.- (minAda <> adaFee))
        , paysPK psFeeToPKH (liquidityShareVal <> minAda)
        ]
  validateTxSkel $ TxSkel (Just parm)
    (def { unsafeModTx = RawModTx $ alterRedeemerAt (fst poolOut) $ computeRedeemerFor ownerRef psFeeTo })
    constr
  where
    minAda = Pl.assetClassValue adaCoin minimumAdaPerUTxO

    adaFee = Pl.assetClassValue adaCoin 10

    liqAC = Pl.assetClass mkLiquiditySymbol (snd $ Pl.unAssetClass wlsPoolNFTClass)

    computeRedeemerFor :: Pl.TxOutRef -> Pl.Address -> [Pl.TxIn] -> [Pl.TxOut] -> PoolRedeemer
    computeRedeemerFor ownerRef payeeAddress txIns txOuts
      = let payeeIx = fromJust $ findIndex ((== payeeAddress) . Pl.txOutAddress) txOuts
            ownerIx = fromJust $ findIndex ((== ownerRef) . Pl.txInRef) txIns
         in WithdrawLiquidityShare (fromIntegral ownerIx) (fromIntegral payeeIx)

-- * Batch Order Validator

-- | Creating orders returns an 'OrderId', which can later be used in
-- 'txApplyPool' in the given order.

type OrderId = Pl.TxId

data Order
  = WithdrawOrder TxCreateWithdrawOrder
  | DepositOrder TxCreateDepositOrder
  | OneSideDepositOrder TxCreateOneSideDepositOrder
  | SwapExactOutOrder TxCreateSwapExactOutOrder
  | SwapExactInOrder TxCreateSwapExactInOrder
  deriving Show

txCreateOrder :: (MonadBlockChain m) => Order -> m OrderId
txCreateOrder (WithdrawOrder w) = txCreateWithdrawOrder w
txCreateOrder (DepositOrder d) = txCreateDepositOrder d
txCreateOrder (OneSideDepositOrder osd) = txCreateOneSideDepositOrder osd
txCreateOrder (SwapExactOutOrder osd) = txCreateSwapExactOutOrder osd
txCreateOrder (SwapExactInOrder osd) = txCreateSwapExactInOrder osd

data OrderType
  = WO | DO | OSDO | SEOO | SEIO
  deriving (Eq, Show)

orderType :: Order -> OrderType
orderType (WithdrawOrder _) = WO
orderType (DepositOrder _) = DO
orderType (OneSideDepositOrder _) = OSDO
orderType (SwapExactInOrder _) = SEIO
orderType (SwapExactOutOrder _) = SEOO

-- ** Withdraw Order

data TxCreateWithdrawOrder = TxCreateWithdrawOrder
  { cwoSelf :: Pl.Address
  , cwoWithdrawMin :: (Integer, Integer)
  , cwoWithdrawLP :: Pl.Value
  } deriving (Show)

-- A Withdraw order exchanges LP Tokens for coinA and coinB
txCreateWithdrawOrder :: (MonadBlockChain m) => TxCreateWithdrawOrder -> m OrderId
txCreateWithdrawOrder parm@TxCreateWithdrawOrder {..} = do
  let fee = 1_000_000
  let withdrawDatum = OrderDatum
        { odSender = cwoSelf
        , odReceiver = cwoSelf
        , odReceiverDatumHash = Nothing
        , odStep = uncurry Withdraw cwoWithdrawMin
        -- This is how much we're willing to pay the batcher. For us, it really doesn't
        -- matter much.
        , odBatcherFee = fee
        , odOutputADA = minimumAdaPerUTxO
        }
  let payValue = cwoWithdrawLP <> Ada.lovelaceValueOf (minimumAdaPerUTxO + fee)
  validateTxConstrLbl parm
    [ PaysScript orderValidator [(withdrawDatum, payValue)]
    ]

-- ** Deposit Order

data TxCreateDepositOrder = TxCreateDepositOrder
  { cdoSelf :: Pl.Address
  , cdoCoinA :: (Pl.AssetClass, Integer)
  , cdoCoinB :: (Pl.AssetClass, Integer)
  , cdoDepositLPMin :: Integer
  } deriving (Show)

txCreateDepositOrder :: (MonadBlockChain m) => TxCreateDepositOrder -> m OrderId
txCreateDepositOrder parm@TxCreateDepositOrder {..} =
  validateTxConstrLbl parm
    [ PaysScript orderValidator [(depositDatum, payValue)]
    ]
  where
    fee = 1_000_000
    depositDatum = OrderDatum
      { odSender = cdoSelf
      , odReceiver = cdoSelf
      , odReceiverDatumHash = Nothing
      , odStep = Deposit cdoDepositLPMin
      , odBatcherFee = fee
      , odOutputADA = minimumAdaPerUTxO
      }
    payValue = Ada.lovelaceValueOf (minimumAdaPerUTxO + fee)
            <> uncurry Pl.assetClassValue cdoCoinA
            <> uncurry Pl.assetClassValue cdoCoinB

-- ** One Side Deposit

data TxCreateOneSideDepositOrder = TxCreateOneSideDepositOrder
  { cosdSelf :: Pl.Address
  -- | Which coin do we want to receive?
  , cosdDesiredCoin :: Pl.AssetClass
  -- | Which coin are we giving to the pool?
  , cosdCoin :: (Pl.AssetClass, Integer)
  , cosdDepositLPMin :: Integer
  } deriving (Show)

txCreateOneSideDepositOrder :: (MonadBlockChain m) => TxCreateOneSideDepositOrder -> m OrderId
txCreateOneSideDepositOrder parm@TxCreateOneSideDepositOrder {..} =
  validateTxConstrLbl parm
    [ PaysScript orderValidator [(depositDatum , payValue)]
    ]
  where
    fee = 1_000_000
    depositDatum = OrderDatum
      { odSender = cosdSelf
      , odReceiver = cosdSelf
      , odReceiverDatumHash = Nothing
      , odStep = OneSideDeposit cosdDesiredCoin cosdDepositLPMin
      , odBatcherFee = fee
      , odOutputADA = minimumAdaPerUTxO
      }
    payValue = Ada.lovelaceValueOf (minimumAdaPerUTxO + fee)
            <> uncurry Pl.assetClassValue cosdCoin

-- ** Swap Exact Out

-- |Swaps an exact amount of 'cseoCoin' for an exact amount of 'cseoDesiredCoin'.
data TxCreateSwapExactOutOrder = TxCreateSwapExactOutOrder
  { cseoSelf :: Pl.Address
  -- | Which coin do we want to receive?
  , cseoDesiredCoin :: Pl.AssetClass
  -- | Which coin are we giving to the pool?
  , cseoCoin :: (Pl.AssetClass, Integer)
  , cseoExpectedReceive :: Integer
  } deriving (Show)

txCreateSwapExactOutOrder :: (MonadBlockChain m) => TxCreateSwapExactOutOrder -> m OrderId
txCreateSwapExactOutOrder parm@TxCreateSwapExactOutOrder {..} =
  validateTxConstrLbl parm
    [ PaysScript orderValidator [(depositDatum , payValue)]
    ]
  where
    fee = 1_000_000
    depositDatum = OrderDatum
      { odSender = cseoSelf
      , odReceiver = cseoSelf
      , odReceiverDatumHash = Nothing
      , odStep = SwapExactOut cseoDesiredCoin cseoExpectedReceive
      , odBatcherFee = fee
      , odOutputADA = minimumAdaPerUTxO
      }
    payValue = Ada.lovelaceValueOf (minimumAdaPerUTxO + fee)
            <> uncurry Pl.assetClassValue cseoCoin

-- ** Swap Exect In

-- |Swaps a fixed amount of 'cseiCoin' for a minimum amount of 'cseiDesiredCoin'
data TxCreateSwapExactInOrder = TxCreateSwapExactInOrder
  { cseiSelf :: Pl.Address
  -- | Which coin do we want to receive?
  , cseiDesiredCoin :: Pl.AssetClass
  -- | Which coin are we giving to the pool?
  , cseiCoin :: (Pl.AssetClass, Integer)
  , cseiMinimumReceive :: Integer
  } deriving (Show)

txCreateSwapExactInOrder :: (MonadBlockChain m) => TxCreateSwapExactInOrder -> m OrderId
txCreateSwapExactInOrder parm@TxCreateSwapExactInOrder {..} =
  validateTxConstrLbl parm
    [ PaysScript orderValidator [(depositDatum , payValue)]
    ]
  where
    fee = 1_000_000
    depositDatum = OrderDatum
      { odSender = cseiSelf
      , odReceiver = cseiSelf
      , odReceiverDatumHash = Nothing
      , odStep = SwapExactIn cseiDesiredCoin cseiMinimumReceive
      , odBatcherFee = fee
      , odOutputADA = minimumAdaPerUTxO
      }
    payValue = Ada.lovelaceValueOf (minimumAdaPerUTxO + fee)
            <> uncurry Pl.assetClassValue cseiCoin

-- * Applying Orders

data TxApplyOrder = TxApplyOrder
  { -- | The NFT identifying the pool we want to interact with; AFAIU, this should
    -- uniquely identify a pool UTxO.
    aoPoolNFTClass :: Pl.AssetClass

    -- We also need to pass which orders we want executed. Orders do get executed in
    -- lex order, so we have to sort this.
  , ordersToApply :: [OrderId]
  }
  deriving Show

txApplyOrders :: forall m . (MonadBlockChain m) => TxApplyOrder -> m Pl.TxId
txApplyOrders = txApplyOrders' IgnoreRefunds

txApplyOrders' :: forall m . (MonadBlockChain m) => RefundPolicy -> TxApplyOrder -> m Pl.TxId
txApplyOrders' refunds parm@TxApplyOrder{..} = do
  -- Get our address and find our license
  pkh <- ownPaymentPubKeyHash
  let myAddr = Pl.Address (Pl.PubKeyCredential pkh) Nothing
  license : _ <- pkUtxosSuchThatValue pkh (hasCurrencySymbol mkLicenseSymbol)

  -- Find the pool
  [(poolOut, poolDat)] <- scriptUtxosSuchThat poolValidator (\_ -> hasAssetClass aoPoolNFTClass)

  -- Sort the orders
  let sortedOrdersToApply = sort ordersToApply

  -- Find the orders and filter those that are not supposed to be processed;
  -- note that we make sure to find all the orders we want to process here or error if
  -- miss one.
  --
  -- We also return the list of ordersToProcess in the same order as the caller requested.
  orders <- scriptUtxosSuchThat orderValidator (\ _ _ -> True)
  ordersToProcess <- case mapM (\oid -> find ((== oid) . Pl.txOutRefId . fst . fst) orders) sortedOrdersToApply of
                      Just os -> return os
                      Nothing -> fail "Didn't find one of the orders to apply"

  -- Now we process the minswap-orders in order.
  let ps0 = poolDatumToState (poolOut, poolDat) liqAC
  (constr, ps, batcherFee) <- foldM (orderFolder poolDat) (mempty, ps0, 0) ordersToProcess

  let (poolDat', val) = updatePoolDatum ps aoPoolNFTClass poolDat
  let liqShareVal = Pl.assetClassValue liqAC $ psLiquidityShare ps
  let val' = val <> liqShareVal

  let totalDeltaLiquidity = pdTotalLiquidity poolDat' - pdTotalLiquidity poolDat

  now <- currentTime
  -- The base constraints are consuming and producing a poolValidator:

  let baseConstr =
        -- These magic numbers in ApplyPool refer to the index of the order(s) being
        -- batched in the Tx and to the index of the input containing the license in the tx
        [ SpendsScript poolValidator (ApplyPool myAddr 0) (poolOut, poolDat)
        , PaysScript poolValidator [(poolDat', val')]
        , SpendsPK license
        , paysPK pkh ((sOutValue license Pl.- adaFee) <> Ada.lovelaceValueOf batcherFee)
        , Before (now + 100000)
        , mints [lpMP] (Pl.assetClassValue liqAC totalDeltaLiquidity)
        ]
  validateTxSkel $ TxSkel (Just parm)
    (def { adjustUnbalTx = True
         , unsafeModTx = RawModTx $
             alterRedeemerAt (fst poolOut)
               (computeRedeemerFor myAddr (fst license)) })
    (baseConstr ++ constr)
 where
   orderFolder poolDat (constr, ps, batcherFee) orderOutDat = do
     -- orderToConstr only takes the constant parts of the pool datum,
     -- so it's fine-ish to use the old `poolDat`,
     -- which is not in sync with the accumulator `ps`.
     --
     -- I don't like it, but it's probably better to keep it this way
     -- for consistency with the computeValid* family of functions.
     (orderConstr, ps', batcherFee') <- orderToConstr poolDat orderOutDat ps
     pure (orderConstr ++ constr, ps', batcherFee + batcherFee')

   adaFee = Pl.assetClassValue adaCoin 10

   liqAC = Pl.assetClass mkLiquiditySymbol (snd $ Pl.unAssetClass aoPoolNFTClass)

   orderToConstr :: PoolDatum -> (SpendableOut, OrderDatum) -> PoolState -> m ([Constraint], PoolState, Integer)
   orderToConstr pd (sout, od) ps = interpretOrderResult refunds sout od $
      case odStep od of
           Withdraw {..} ->
             computeValidWithdraw (sOutValue sout) od ps pd liqAC wMinimumCoinA wMinimumCoinB
           Deposit {..} ->
             computeValidDeposit (sOutValue sout) od ps pd liqAC dMinimumLP
           OneSideDeposit {..} ->
             computeValidOneSideDeposit (sOutValue sout) od ps pd liqAC osdDesiredCoin osdMinimumLP
           SwapExactOut {..} ->
             computeValidSwapExactOut (sOutValue sout) od ps pd seoDesiredCoin seoExpectedReceive
           SwapExactIn {..} ->
             computeValidSwapExactIn (sOutValue sout) od ps pd seiDesiredCoin seiMinimumReceive

   -- The list of inputs we get here is assumed to be sorted; check the txIns vars
   -- in @alterRedeemerAt@. Another complex nuance is that the licenseIx is an index
   -- into the list of all txIns; but the ordersIxs are indexes into a filtered list.
   -- Check Minswap.ConstantProductPool.OnChain:284 for where I found it out.
   computeRedeemerFor :: Pl.Address -> Pl.TxOutRef -> [Pl.TxIn] -> outs -> PoolRedeemer
   computeRedeemerFor myAddr licenseOutRef txIns _
     = let licenseIx = fromJust $ findIndex ((== licenseOutRef) . Pl.txInRef) txIns
        in ApplyPool myAddr (fromIntegral licenseIx)

alterRedeemerAt :: (Pl.ToData a) => Pl.TxOutRef -> ([Pl.TxIn] -> [Pl.TxOut] -> a) -> Pl.Tx -> Pl.Tx
alterRedeemerAt poolOutRef f tx = tx { Pl.txInputs = S.fromList $ map go txIns }
  where
    txIns = S.toList (Pl.txInputs tx)

    go :: Pl.TxIn -> Pl.TxIn
    go txIn
      | Pl.txInRef txIn == poolOutRef =
        Pl.TxIn poolOutRef (setRedeemer (f txIns $ Pl.txOutputs tx) $ Pl.txInType txIn)
      | otherwise = txIn

    setRedeemer :: (Pl.ToData a) => a -> Maybe Pl.TxInType -> Maybe Pl.TxInType
    setRedeemer x (Just (Pl.ConsumeScriptAddress v _ d)) =
      Just (Pl.ConsumeScriptAddress v (Pl.Redeemer $ Pl.toBuiltinData x) d)
    setRedeemer _ _ = error "This is supposed to be called from a ConsumeScriptAddress"
