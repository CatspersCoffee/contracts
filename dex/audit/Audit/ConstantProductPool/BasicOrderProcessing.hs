{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This module collects a series of basic tests to estabilish the baseline of
-- behaviors we need from off-chain rewrite using @cooked-validators@,
-- making sure we understand MinSwap's operations and design.
module Audit.ConstantProductPool.BasicOrderProcessing where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Test.Tasty.QuickCheck (testProperty, QuickCheckTests(..))
import Test.QuickCheck (Property, choose)
import Control.Monad
import Control.Applicative
import Data.List (sortBy)

import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Ledger.Value as Pl
import qualified Ledger.Ada as Pl
import Minswap.ConstantProductLiquidity.OnChain (mkLiquiditySymbol)
import Minswap.ConstantProductPool.OnChain (mkLicenseSymbol)
import Minswap.ConstantProductPool.Utils
import Minswap.Utils.OnChainUtils

import Audit.OffChain
import Audit.ConstantProductPool.OrderGen

tests :: TestTree
tests = testGroup "Apply Basic Orders"
  [ testGroup "Unit tests: one order at a time"
    [ testCase "deposit order" $ testSucceedsFrom i0 trBasicDeposit
    , testCase "withdraw order" $ testSucceedsFrom i0 trBasicWithdraw
    , testCase "one-side deposit order" $ testSucceedsFrom i0 trBasicOneSideDeposit
    , testCase "swap-exact-out order" $ testSucceedsFrom i0 trBasicSwapExactOut
    , testCase "swap-exact-in order" $ testSucceedsFrom i0 trBasicSwapExactIn
    ]
  , testCase "Unit test: price increases on successive orders" $
      let priceIncr :: [Integer] -> Assertion
          priceIncr deltas = assertBool ("Deltas not in descending order: " ++ show deltas)
                           $ deltas == sortBy (flip compare) deltas

          test :: (Show err) => Either err ([Integer], UtxoState) -> Assertion
          test (Left err) = assertFailure $ "expected success: " ++ show err
          test (Right (is, _)) = priceIncr is

          pool = TxCreatePool (quickAssetClass "XXX", 1_000_000) (quickAssetClass "YYY", 1_000_000)
       in testAllSatisfiesFrom test i0 (createExecuteAndTrackPrice pool 8)
  -- This is a scenario that we know is possible; we assume that batchers won't be doing
  -- these type of operations, but we wanted to study how easy it would be. Naturally, one can assume
  -- that the tighter the /minimumReceive/ the less chance a batcher has to perform this attack.
  , expectFail $ testCase "Unit test: dishonest batcher can use custom pool" $
        let honestPool = TxCreatePool (quickAssetClass "XXX", 10_000_000) (quickAssetClass "YYY", 10_000_000)
            sei = TxCreateSwapExactInOrder (walletAddress $ wallet 2) (quickAssetClass "YYY")
                                           (quickAssetClass "XXX", 40_000) 20_000

         in testBinaryRelatedBy equalModuloAda i0 (trDishonestBatcherCustomPool honestPool sei)
  , testGroup "Property tests"
    [ testProperty "generated order succeeds" $
      forAll (genPool 10_000_000 >>= genValidOrderFromI0)
        (\ (pool, ord, w) -> testSucceedsFrom i0 $ createAndExecuteSingle pool (orderGen ord) w)

    , localOption (QuickCheckTests 5) $ testProperty "Price increases" $
      forAll ((,) <$> genPool 10_000_000 <*> choose (5, 15))
        (testSucceedsFrom i0 . uncurry createExecuteAndTrackPrice)
    ]
  , multiplePoolsTest
  ]

multiplePoolsTest :: TestTree
multiplePoolsTest = testGroup "Multiple Pools"
    [ testProperty "batcher can batch on the pool chosen by the order author" $
        forAll generator (testSucceedsFrom i0 . runner False)
      -- documented as uncl:pool-choice
    , testProperty "batcher can't batch on a different pool (!)" $
        forAll generator
          (testCounterexample "batcher was able to swap the pools" . testFailsFrom i0 . runner True)
    ]
  where
    generator = do let maxAssets = 5_000_000
                   pool1@(TxCreatePool (c1, _) (c2, _)) <- genPool maxAssets
                   (_, ord, ordW) <- genValidOrderFromI0 pool1
                   max1' <- genPoolCoinAmount maxAssets
                   max2' <- genPoolCoinAmount maxAssets
                   let pool2 = TxCreatePool (c1, max1') (c2, max2')
                   pure (pool1, pool2, ord, ordW)
    runner swap (pool1, pool2, ord, orderW) = do
        p1 <- createAndExecute pool1 False []
        p2 <- createAndExecute pool2 False []

        let liqAC = Pl.assetClass mkLiquiditySymbol (snd $ Pl.unAssetClass p1)
            order = orderGen ord liqAC

        oid <- txCreateOrder order `as` orderW

        txApplyOrders (TxApplyOrder (if swap then p2 else p1) [oid]) `as` wBatcher


trBasicDeposit :: (MonadMockChain m) => m ()
trBasicDeposit = createPoolThenApplyOneOrder (const order) (wallet 2)
  where
    order = DepositOrder $ TxCreateDepositOrder
              { cdoSelf = walletAddress $ wallet 2
              , cdoCoinA = (quickAssetClass "XXX" , 1_000)
              , cdoCoinB = (quickAssetClass "YYY" , 1_000)
              , cdoDepositLPMin = 0 -- TODO calculate
              }

trBasicOneSideDeposit :: (MonadMockChain m) => m ()
trBasicOneSideDeposit = createPoolThenApplyOneOrder (const order) (wallet 2)
  where
    order = OneSideDepositOrder $ TxCreateOneSideDepositOrder
              { cosdSelf = walletAddress $ wallet 2
              , cosdDesiredCoin = quickAssetClass "YYY"
              , cosdCoin = (quickAssetClass "XXX" , 1_000)
              , cosdDepositLPMin = 0 -- TODO calculate
              }

trBasicWithdraw :: (MonadMockChain m) => m ()
trBasicWithdraw = createPoolThenApplyOneOrder order (wallet 1)
  where
    order ac = WithdrawOrder $ TxCreateWithdrawOrder
                { cwoSelf = walletAddress $ wallet 1
                , cwoWithdrawMin = (1, 1)
                , cwoWithdrawLP = Pl.assetClassValue ac 200
                }

trBasicSwapExactOut :: (MonadMockChain m) => m ()
trBasicSwapExactOut = createPoolThenApplyOneOrder (const order) (wallet 2)
  where
    order = SwapExactOutOrder $ TxCreateSwapExactOutOrder
              { cseoSelf = walletAddress $ wallet 2
              , cseoDesiredCoin = quickAssetClass "YYY"
              , cseoCoin = (quickAssetClass "XXX" , 1_000)
              , cseoExpectedReceive = 996
              }

trBasicSwapExactIn :: (MonadMockChain m) => m ()
trBasicSwapExactIn = createPoolThenApplyOneOrder (const order) (wallet 2)
  where
    order = SwapExactInOrder $ TxCreateSwapExactInOrder
              { cseiSelf = walletAddress $ wallet 2
              , cseiDesiredCoin = quickAssetClass "YYY"
              , cseiCoin = (quickAssetClass "XXX" , 1_000)
              , cseiMinimumReceive = 996
              }

createPoolThenApplyOneOrder :: (MonadMockChain m) => (Pl.AssetClass -> Order) -> Wallet -> m ()
createPoolThenApplyOneOrder = createAndExecuteSingle pool
  where
    pool = TxCreatePool (quickAssetClass "XXX", 1_000_000)
                        (quickAssetClass "YYY", 1_000_000)

-- Execute a SwapExactInOrder a number of times and return the progressive amount of
-- tokens we received as a result of that. We expect that list to be sorted in
-- descending order; that is, we should be getting less and less coinOuts per trade
-- since the exchange rate should be getting worse and worse.
createExecuteAndTrackPrice :: (MonadMockChain m) => TxCreatePool -> Integer ->  m [Integer]
createExecuteAndTrackPrice pool maxRuns = do
  (nftAC, _) <- txCreatePool pool `as` wallet 1
  -- Keep the initial reserves,
  initReserves <- walletReservesOf coinOut
  -- Now execute a number of repeated trades in sequence recording our new reserves of coinOut
  successiveReserves <- mapM (\ _ -> go nftAC >> walletReservesOf coinOut) [1 .. maxRuns]
  -- Finally, return their pointwise difference
  return $ zipWith (-) successiveReserves (initReserves : successiveReserves)
  where
    receiver = wallet 4
    sender = wallet 2

    coinIn = fst (tcpCoinA pool)
    coinOut = fst (tcpCoinB pool)

    walletReservesOf ac = do
      outs <- pkUtxosSuchThatValue (walletPKHash receiver) $ hasAssetClass ac
      return $ flip Pl.assetClassValueOf ac $ mconcat $ map sOutValue outs

    go nftAC = void $ do
      oid <- txCreateSwapExactInOrder sei `as` sender
      txApplyOrders (TxApplyOrder nftAC [oid]) `as` wBatcher
      where
        sei = TxCreateSwapExactInOrder
                { cseiSelf = walletAddress receiver
                , cseiDesiredCoin = coinOut
                , cseiCoin = (coinIn, 1_000)
                , cseiMinimumReceive = 1
                }

-- | Takes a pool and a 'OneSideDeposit' order for that pool, to be submited by @wallet 2@,
-- and executes said order in a newly created, much more unfavorable pool, created by @wallet 3@.
-- In order to choose the parameters for the creation of this new pool, we hacked together a
-- quick binary-search like iterative algorithm. It is sufficient to illustrate the concept.
trDishonestBatcherCustomPool :: (MonadMockChain m, Alternative m)
                             => TxCreatePool
                             -> TxCreateSwapExactInOrder
                             -> m ()
trDishonestBatcherCustomPool pool sei = void $ do
  (nftAC, _) <- txCreatePool pool `as` wallet 1
  oid <- txCreateSwapExactInOrder sei `as` wallet 2

  -- Now, we'll make a pool that we control in case we're a dishonestBatcher
  nft <- return nftAC
     <|> do
          let res = calc (snd $ cseiCoin sei) (cseiMinimumReceive sei)
          let (ammA0, ammB0) = if fst (tcpCoinA pool) == fst (cseiCoin sei) then res else swap res
          let pool2 = TxCreatePool (fst $ tcpCoinA pool, ammA0) (fst $ tcpCoinB pool, ammB0)
          (nftAC2, _) <- txCreatePool pool2 `as` wallet 3
          return nftAC2

  txApplyOrders (TxApplyOrder nft [oid]) `as` wBatcher

  where
    swap (x, y) = (y, x)

    works deltaIn minReceive i o = getAmountOut i o deltaIn >= minReceive

    -- f x1 y1 is guaranteed to return true; what is the minimum x and y
    -- in their respective intervals such that f returns true?
    minimize2 0    _ (_,  x1) (_,  y1) = (x1, y1)
    minimize2 fuel f (x0, x1) (y0, y1) =
      let x = x0 + (x1 - x0) `div` 2
          y = y0 + (y1 - y0) `div` 2
       in if| f x y     -> minimize2 (fuel - 1) f (x0, x) (y0, y)
            | f x1 y    -> minimize2 (fuel - 1) f (x, x1) (y0, y)
            | f x y1    -> minimize2 (fuel - 1) f (x0, x) (y, y1)
            | otherwise -> minimize2 (fuel - 1) f (x, x1) (y, y1)

    calc deltaIn minReceive =
      let range = (minReceive, 10_000_000)
       in minimize2 (20 :: Int) (works deltaIn minReceive) range range

-- Initial distribution for all the tests in this module
i0 :: InitialDistribution
i0 = customInitialDistribution <> distributionFromList
       [(wallet 1, quickValue "XXX" 10_000_000
                <> quickValue "YYY" 10_000_000)
       ,(wallet 2, quickValue "XXX" 10_000_000
                <> quickValue "YYY" 10_000_000)
       ,(wallet 3, quickValue "XXX" 10_000_000
                <> quickValue "YYY" 10_000_000)
       ,(wBatcher, Pl.singleton mkLicenseSymbol license 1)
       ]
  where
    -- This is a license valid until 03-Apr-2022
    license = Pl.TokenName $ integerToBS 1648997079000

    -- Additionally, we'll bump everyone's lovelace count because
    -- when batching lots of orders, especiall on pools that also
    -- trade ADA, the default amount of 100 ada gets spent really fast.
    customInitialDistribution :: InitialDistribution
    customInitialDistribution = distributionFromList $ zip knownWallets (repeat def)
      where
        def = Pl.lovelaceValueOf 1_000_000_000
