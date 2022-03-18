{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- |Provides facilities for generating orders with QuickCheck.
module Audit.ConstantProductPool.OrderGen where

import Control.Monad
import Test.QuickCheck (Gen, oneof, choose, elements, discard)

import Cooked.MockChain
import qualified Ledger.Value as Pl
import Minswap.ConstantProductPool.Utils
import Minswap.ConstantProductPool.Types
import Minswap.ConstantProductLiquidity.OnChain

import Audit.OffChain

-- * Order Generation
--
-- Because orders might need to specify the asset class they
-- operate over, and because the asset class for a pool's LP token
-- depends on the pool's NFT, we will only have this information at runtime.
-- Therefore, instead of generating simple a 'Order', we generate
-- functions 'Pl.AssetClass -> Order'.

-- This new type is needed to derive Show; which is useful to have around
-- when something fails.
newtype OrderGen = OrderGen { orderGen :: Pl.AssetClass -> Order }
instance Show OrderGen where
  show (OrderGen g) = unlines
    ["<generated with 'quickAssetClass \"XXX\"'>"
    , show $ g (quickAssetClass "XXX")
    ]

unwrapOrderGens :: [(OrderGen, b)] -> [Pl.AssetClass -> (Order, b)]
unwrapOrderGens = map (\(og, w) ac -> (orderGen og ac, w))

runOrderGensInPool :: (MonadMockChain m) => Pl.AssetClass -> [(OrderGen, Wallet)] -> m ()
runOrderGensInPool nftAC orderGens = do
  oids <- forM (unwrapOrderGens orderGens) $ \gen
           -> let (order, orderW) = gen liqAC
              in txCreateOrder order `as` orderW
  unless (null oids) $ void $ txApplyOrders (TxApplyOrder nftAC oids) `as` wBatcher
  where
    liqAC = Pl.assetClass mkLiquiditySymbol (snd $ Pl.unAssetClass nftAC)

genPoolCoinAmount :: Integer -> Gen Integer
genPoolCoinAmount maxPoolAmm = choose (maxPoolAmm `div` 2 , maxPoolAmm)

genPool :: Integer -> Gen TxCreatePool
genPool maxPoolAmm = do
  ammA <- genPoolCoinAmount maxPoolAmm
  ammB <- genPoolCoinAmount maxPoolAmm
  (a, b) <- oneof $ map return $
    [ (coinA , coinB) | coinA <- coins, coinB <- coins, coinA < coinB ]
  return $ TxCreatePool (quickAssetClass a, ammA) (quickAssetClass b, ammB)
  where
    coins = ["", "XXX", "YYY"]

commonParamsGen :: Int -> Gen (TxCreatePool, [(OrderGen, Wallet)], Bool)
commonParamsGen maxOrders = do
  pool <- genPool 10_000_000
  n <- choose (1, maxOrders)
  orders <- genOrdersFrom n (initAbsPoolState pool)
  profitSharing <- elements [True, False]
  return (pool, orders, profitSharing)
  where
    genOrdersFrom 0 _ = return []
    genOrdersFrom n ps = do
      (og, w, ps') <- genValidOrderFromI0' ps
      ((og, w) :) <$> genOrdersFrom (n-1) ps'

-- | Generates orders that can be filled from a state where
-- we start from i0 and wallet1 opens up a pool with 1_000_000 of
-- each "XXX" and "YYY"; orders are to be executed against that pool.
genValidOrderFromI0 :: TxCreatePool -> Gen (TxCreatePool, OrderGen, Wallet)
genValidOrderFromI0 pool = do
  (order, w, _) <- genValidOrderFromI0' (initAbsPoolState pool)
  return (pool, order, w)

genValidOrderFromI0' :: AbsPoolState -> Gen (OrderGen, Wallet, AbsPoolState)
genValidOrderFromI0' tx =
  first3 OrderGen <$> oneof
             [ stdWallets >>= genDepositOrder tx
             , stdWallets >>= genOneSideDepositOrder tx
             , stdWallets >>= genSwapExactOut tx
             , stdWallets >>= genSwapExactIn tx
             -- the order sender has to be wallet 1, since they're the only one
             -- with LP tokens at this point.
             , stdReceiver >>= genWithdraw tx . (, wallet 1)
             ]
  where
    first3 f (x, y, z) = (f x, y , z)
    stdReceiver = wallet <$> choose (1, 8)
    stdSender = wallet <$> choose (2, 3)
    stdWallets = (,) <$> stdReceiver <*> stdSender

genInteger :: Gen Integer
genInteger = choose (100, 1_000)

type OrderArbitrary = AbsPoolState -> (Wallet, Wallet) -> Gen (Pl.AssetClass -> Order, Wallet, AbsPoolState)

genDepositOrder :: OrderArbitrary
genDepositOrder ps@AbsPoolState{..} (orderReceiver, orderSender) = do
  ord <- TxCreateDepositOrder (walletAddress orderReceiver)
          <$> ((tokenA,) <$> genInteger)
          <*> ((tokenB,) <$> genInteger)
          <*> return 0
  case executeDep ps ord of
    Nothing -> discard
    Just ps' -> return (DepositOrder . const ord, orderSender, ps')

genOneSideDepositOrder :: OrderArbitrary
genOneSideDepositOrder ps@AbsPoolState{..} (orderReceiver, orderSender) = do
  (desired, tgt) <- oneof $ map return [(tokenA, tokenB), (tokenB, tokenA)]
  ord <- TxCreateOneSideDepositOrder
          (walletAddress orderReceiver)
          desired
          <$> ((tgt,) <$> genInteger)
          <*> return 0
  case executeOneSideDep ps ord of
    Nothing -> discard
    Just ps' -> return (OneSideDepositOrder . const ord, orderSender, ps')

genSwapExactOut :: OrderArbitrary
genSwapExactOut ps@AbsPoolState{..} (orderReceiver, orderSender) = do
  (desired, tgt) <- oneof $ map return [(tokenA, tokenB), (tokenB, tokenA)]
  amm <- genInteger
  let (mB, mA) = if desired == tokenA then (ammA, ammB) else (ammB, ammA)
  let expectedReceive = getAmountOut mA mB amm
  let ord = TxCreateSwapExactOutOrder
              (walletAddress orderReceiver)
              desired
              (tgt, amm)
              expectedReceive
  case executeSwapExactOut ps ord of
    Nothing -> discard
    Just ps' -> return (SwapExactOutOrder . const ord, orderSender, ps')

genSwapExactIn :: OrderArbitrary
genSwapExactIn ps@AbsPoolState{..} (orderReceiver, orderSender) = do
  (desired, tgt) <- oneof $ map return [(tokenA, tokenB), (tokenB, tokenA)]
  amm <- genInteger
  let (mB, mA) = if desired == tokenA then (ammA, ammB) else (ammB, ammA)
  let expectedReceive = getAmountOut mA mB amm
  let ord = TxCreateSwapExactInOrder
              (walletAddress orderReceiver)
              desired
              (tgt, amm)
              expectedReceive
  case executeSwapExactIn ps ord of
    Nothing -> discard
    Just ps' -> return (SwapExactInOrder . const ord, orderSender, ps')

genWithdraw :: AbsPoolState -> (Wallet, Wallet) -> Gen (Pl.AssetClass -> Order, Wallet, AbsPoolState)
genWithdraw ps (orderReceiver, orderSender) = do
  amm <- genInteger
  let ord ac = TxCreateWithdrawOrder
              (walletAddress orderReceiver)
              (1, 1) -- min withdraw
              (Pl.assetClassValue ac amm)
  case executeWithdraw ps ord of
    Nothing -> discard
    Just ps' -> return (WithdrawOrder . ord, orderSender, ps')


-- |The initial TxCreatePool is equivalent to the following pool state:
initAbsPoolState :: TxCreatePool -> AbsPoolState
initAbsPoolState p0 =
  let PoolDatum {..} = initialPoolDatum p0
      TxCreatePool {..} = sortTxCreatePool p0
   in AbsPoolState (fst tcpCoinA) (fst tcpCoinB) (snd tcpCoinA) (snd tcpCoinB) pdTotalLiquidity

-- * Generating Unrefundable Orders
--
-- Currently, we're just ignoreing orders that would be refunded, simulating the
-- off-chain code from MinSwap. If we ever want to change that, we can rely on
-- using the `AbsPoolState` in all the generators above and on the predicates
-- below to be able to discard orders that won't be applicable.

data AbsPoolState = AbsPoolState
  { tokenA :: Pl.AssetClass
  , tokenB :: Pl.AssetClass
  , ammA :: Integer
  , ammB :: Integer
  , liq :: Integer
  }

executeDep :: AbsPoolState -> TxCreateDepositOrder -> Maybe AbsPoolState
executeDep ps _ = Just ps

executeOneSideDep :: AbsPoolState -> TxCreateOneSideDepositOrder -> Maybe AbsPoolState
executeOneSideDep ps _ = Just ps

executeSwapExactIn :: AbsPoolState -> TxCreateSwapExactInOrder -> Maybe AbsPoolState
executeSwapExactIn ps _ = Just ps

executeSwapExactOut :: AbsPoolState -> TxCreateSwapExactOutOrder -> Maybe AbsPoolState
executeSwapExactOut ps _ = Just ps

executeWithdraw :: AbsPoolState -> (Pl.AssetClass -> TxCreateWithdrawOrder) -> Maybe AbsPoolState
executeWithdraw ps _ = Just ps
