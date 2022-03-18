{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

-- | This module tests the use of pools with profit-sharing turned on and makes
-- sure that their operation succeeds.
module Audit.ConstantProductPool.ProfitSharing where

import Cooked.MockChain

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Control.Monad

import qualified Ledger.Value as Pl
import Minswap.ConstantProductPool.OnChain (mkLicenseSymbol, mkOwnerTokenName)
import Minswap.ConstantProductPool.Types

import Audit.OffChain
import qualified Audit.ConstantProductPool.BasicOrderProcessing as BOP
import Audit.ConstantProductPool.OrderGen

tests :: TestTree
tests = testGroup "Profit Sharing"
  [ testGroup "Unit tests"
    [ testCase "can turn it on" $ testSucceedsFrom i0 trCreateShareablePool
    ]
  , testGroup "Property tests"
    [ testProperty "orders can be processed with profit-sharing on" $
        forAll (genPool 10_000_000 >>= genValidOrderFromI0)
          (\ (pool, ord, w) -> testSucceedsFrom i0 $ createAndExecute pool True [\ac -> (orderGen ord ac, w)])
    ]
  ]

trCreateShareablePool :: (MonadMockChain m) => m Pl.AssetClass
trCreateShareablePool = do
  (nftAC, _) <- txCreatePool pool `as` wallet 1
  void $ txUpdateFeeTo (feeUpd8 nftAC) `as` wMinSwap
  pure nftAC
  where
    pool = TxCreatePool (quickAssetClass "XXX", 1_000_000)
                        (quickAssetClass "YYY", 1_000_000)

    feeUpd8 ac = TxUpdateFeeTo ac (Just $ ProfitSharing (walletAddress wMinSwap) Nothing)

-- Initial distribution for all the tests in this module; consists in the basic
-- initial distribution but with some distinguished "Owner" wallet, which is
-- the only wallet that can turn profit-sharing on
i0 :: InitialDistribution
i0 = BOP.i0 <> distributionFromList
       [(wMinSwap, Pl.singleton mkLicenseSymbol mkOwnerTokenName 1)]
