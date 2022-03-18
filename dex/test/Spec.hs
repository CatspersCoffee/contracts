module Main
  ( main,
  )
where

import qualified Spec.Utils
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Minswap DEX"
    [Spec.Utils.tests]
