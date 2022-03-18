{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

-- | This module is concerned with batching orders together and with executing multiple batches in sequence.
module Audit.ConstantProductPool.Batching where

import Cooked.MockChain

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, Gen, choose, tabulate, Property)
import Test.Tasty.ExpectedFailure
import Control.Monad
import Control.Applicative

import Audit.OffChain
import Audit.ConstantProductPool.OrderGen
import qualified Audit.ConstantProductPool.BasicOrderProcessing as BOP
import qualified Audit.ConstantProductPool.ProfitSharing as PS

tests :: TestTree
tests = testGroup "Batching"
  [ testGroup "Unit tests"
      [ testCase "two deposits" $ testSucceedsFrom BOP.i0 trDepositDeposit
      ]
   , testGroup "Property tests"
      -- First we test that a single call to applyOrders with a large list of orders succeeds.
      [ testProperty ("can batch up to " ++ show maxOrders ++ " orders at once") $
          -- Note here we go from the ProfitSharing initial distr; since we want to
          -- be able to set profit-sharing on and off.
          let recordOrderTypes (_, otys) _ = tabulate "Order Types" (map show otys) (testSuccess :: Property)
           in forAll
            -- Gen parameters
            (commonParamsGen maxOrders)
            -- Run test
            (\(pool, orders, ps) ->
              testSucceedsFrom' recordOrderTypes PS.i0 $ createAndExecute' pool ps $ unwrapOrderGens orders)

      -- Now we check that successive calls to applyOrders also succeeds. Note that we're still
      -- not checking that the result is related to not associating it at all.
      , testProperty ("can batch up to " ++ show maxOrders ++ " in different associations") $
          forAll
            (commonParamsGenAssoc maxOrders)
            (\(pool, partitionedOrders, ps) -> testSucceedsFrom PS.i0 $ do
                -- run with an empty order list to only create a pool and set up profitSharing,
                -- according to ps.
                nftAC <- createAndExecute pool ps []
                mapM_ (runOrderGensInPool nftAC) partitionedOrders
            )

      ]
   -- There are three tests that are now obsolete. When these tests were engineered, batchers
   -- would process orders in a specific sequence. This is no longer the case, orders are now
   -- processes after being sorted lexicographically on TxId, hence the sequence in which they
   -- are processed is fixed. Therefore, order processing is no longer associative. Take:
   --
   -- t1 = applyOrders [a,c] >> applyOrders [b,d]
   -- t2 = applyOrders [a,c,b,d]
   --
   -- In t2, the off-chain will reorder the list lexicographically, and will actually process
   -- the list [a,b,c,d] of orders, which will yield a different result than t1.
   --
   -- Additionally, there was a concern about batchers being able to permute order lists which
   -- also no longer applies.
   , ignoreTest $ testGroup "Obsolete Tests"
      [ testCase "is associative modulo fees" $
          testBinaryRelatedBy equalModuloAda PS.i0 trAssociativeUnitTest

      -- document as bug:ordering on report.
      , testCase "batchers cannot permute order lists (!)" $
          let predi res = case res of
                [(Right _, _), (Right _, _)] -> assertFailure "Batcher succeeded in processing same list of orders in different ordering"
                _ -> return ()

           in testSatisfiesFrom' predi PS.i0 ((return True <|> return False) >>= trPermuteUnitTest)

      -- Now we run the two options, either we batch all the orders at once or we make smaller batches
      -- and send more transactions. For a list of orders [o1, .. o5], executing:
      --
      -- >   executeOrderInPool pool [o1, o2, o3, o4, o5]
      --
      -- or
      --
      -- >   executeOrderInPool pool [o1, o2]
      -- >   executeOrderInPool pool [o3]
      -- >   executeOrderInPool pool [o4, o5]
      --
      -- Should really be the same modulo fees; note the order doesn't change
      -- what changes is the grouping, only.
      , testProperty "is associative modulo fees" $
          forAll
            (commonParamsGenAssoc maxOrders)
            (\ (pool, partitionedOrders, ps) ->
               testBinaryRelatedBy equalModuloAda PS.i0 $ do
                  nftAC <- createAndExecute pool ps []
                  runOrderGensInPool nftAC (concat partitionedOrders)
                    <|> mapM_ (runOrderGensInPool nftAC) partitionedOrders)
      ]
  ]
  where
    -- VM: Increasing the maxOrders too much will trigger failures with around
    -- ~80 orders. That's way too much for us to look at right now. I have a hunch
    -- that that is caused to public keys just not having enough funds to put 80
    -- parallel orders, but it needs to be double checked.
    maxOrders = 24

-- ** Definition of the Unit-test Traces

trDepositDeposit :: (MonadMockChain m) => m ()
trDepositDeposit = void $ createAndExecute pool False [dep, dep]
  where
    pool = TxCreatePool (quickAssetClass "XXX", 1_000_000)
                        (quickAssetClass "YYY", 1_000_000)

    dep _ = ( DepositOrder $ TxCreateDepositOrder
                  { cdoSelf = walletAddress $ wallet 2
                  , cdoCoinA = (quickAssetClass "XXX" , 500)
                  , cdoCoinB = (quickAssetClass "YYY" , 500)
                  , cdoDepositLPMin = 400
                  }
              , wallet 2
              )


trAssociativeUnitTest :: (Alternative m, MonadMockChain m) => m ()
trAssociativeUnitTest = void $ do
  nftAC <- createAndExecute pool withProfitSharing []
  runOrderGensInPool nftAC (concat orders)
    <|> mapM_ (runOrderGensInPool nftAC) orders
  where
    withProfitSharing = True

    coinA = quickAssetClass ""
    coinB = quickAssetClass "YYY"

    pool = TxCreatePool (coinA, 5812048) (coinB, 5492725)

    orders = [[seo11, osd12], [seo21], [sei31], [sei41, dep42, sei43]]

    seo11 = (OrderGen $ \_ -> SwapExactInOrder $ TxCreateSwapExactInOrder
                 { cseiSelf = walletAddress $ wallet 6
                 , cseiDesiredCoin = coinA
                 , cseiCoin = (coinB,10050)
                 , cseiMinimumReceive = 10583
                 }
              , wallet 2)

    osd12 = (OrderGen $ \_ -> OneSideDepositOrder $ TxCreateOneSideDepositOrder
                { cosdSelf = walletAddress $ wallet 5
                , cosdDesiredCoin = coinB
                , cosdCoin = (coinA,26207)
                , cosdDepositLPMin = 0
                }
              , wallet 3)

    seo21 = (OrderGen $ \_ -> SwapExactOutOrder $ TxCreateSwapExactOutOrder
                { cseoSelf = walletAddress $ wallet 1
                , cseoDesiredCoin = coinB
                , cseoCoin = (coinA,74298)
                , cseoExpectedReceive = 6912
                }
              , wallet 1)

    sei31 = (OrderGen $ \_ -> SwapExactInOrder $ TxCreateSwapExactInOrder
                { cseiSelf = walletAddress $ wallet 5
                , cseiDesiredCoin = coinB
                , cseiCoin = (coinA,16546)
                , cseiMinimumReceive = 1554
                }
              , wallet 2)

    sei41 = (OrderGen $ \_ -> SwapExactInOrder $ TxCreateSwapExactInOrder
                { cseiSelf = walletAddress $ wallet 1
                , cseiDesiredCoin = coinB
                , cseiCoin = (coinA,42932)
                , cseiMinimumReceive = 4015
                }
              , wallet 1)

    dep42 = (OrderGen $ \_ -> DepositOrder $ TxCreateDepositOrder
                { cdoSelf = walletAddress $ wallet 7
                , cdoCoinA = (coinA,94598)
                , cdoCoinB = (coinB,35631)
                , cdoDepositLPMin = 0
                }
              , wallet 2)

    sei43 = (OrderGen $ \_ -> SwapExactInOrder $ TxCreateSwapExactInOrder
                { cseiSelf = walletAddress $ wallet 7
                , cseiDesiredCoin = coinB
                , cseiCoin = (coinA,78063)
                , cseiMinimumReceive = 7258
                }
              , wallet 3)

trPermuteUnitTest :: (MonadMockChain m) => Bool -> m ()
trPermuteUnitTest permute = void $ do
  nftAC <- createAndExecute pool withProfitSharing []
  runOrderGensInPool nftAC $ if permute then reverse orders else orders
  where
    withProfitSharing = True

    coinA = quickAssetClass ""
    coinB = quickAssetClass "YYY"

    pool = TxCreatePool (coinA, 5812048) (coinB, 5492725)

    orders = [dep, sei]

    dep = (OrderGen $ \_ -> DepositOrder $ TxCreateDepositOrder
                { cdoSelf = walletAddress $ wallet 7
                , cdoCoinA = (coinA,94598)
                , cdoCoinB = (coinB,35631)
                , cdoDepositLPMin = 0
                }
              , wallet 2)

    sei = (OrderGen $ \_ -> SwapExactInOrder $ TxCreateSwapExactInOrder
                { cseiSelf = walletAddress $ wallet 1
                , cseiDesiredCoin = coinB
                , cseiCoin = (coinA,42932)
                , cseiMinimumReceive = 40155
                }
              , wallet 1)

-- ** Generators and Auxiliary functions

commonParamsGenAssoc :: Int -> Gen (TxCreatePool, [[(OrderGen, Wallet)]], Bool)
commonParamsGenAssoc maxOrders = do
  (pool, orders, profitSharing) <- commonParamsGen maxOrders
  assocOrders <- partitionList orders
  return (pool, assocOrders, profitSharing)

-- Not the most statistically robust generator
-- as it seems to have a bias for generating larger parts
-- towards the beginning of the partition,
-- but it'll do for our purposes.
partitionList :: [a] -> Gen [[a]]
partitionList [] = pure []
partitionList [x] = pure [ [x] ]
partitionList xs = do
  headLen <- choose (1, length xs)
  let (h, rest) = splitAt headLen xs
  (h :) <$> partitionList rest
