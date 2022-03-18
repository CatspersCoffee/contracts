module Main (main) where

import Test.Tasty

import qualified Audit.ConstantProductPool.BasicOrderProcessing as BasicOrderProcessing
import qualified Audit.ConstantProductPool.Batching as Batching
import qualified Audit.ConstantProductPool.ProfitSharing as ProfitSharing
import qualified Audit.ConstantProductPool.WithdrawLiquidity as WithdrawLiquidity
import qualified Audit.Attacks as Attacks

allTests :: TestTree
allTests = testGroup "Tweag Audit"
  [ BasicOrderProcessing.tests
  , Batching.tests
  , ProfitSharing.tests
  , WithdrawLiquidity.tests
  , Attacks.tests
  ]

main :: IO ()
main = defaultMain allTests
