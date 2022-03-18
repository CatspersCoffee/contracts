{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Utils (tests) where

import Minswap.ConstantProductPool.Utils (calSqrt, calculateInitialLiquidity, getAmountIn, getAmountOut)
import Minswap.Utils.OnChainUtils (base256ToInteger, bsToInteger, integerToBS, integerToBase256)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Test.Tasty
import Test.Tasty.QuickCheck (Positive (Positive))
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "QuickCheck tests"
    [ QC.testProperty "integer <-> base256 ByteString conversion" prop_integer_base256,
      QC.testProperty "integer <-> ASCII ByteString converter reflection" prop_integer_bs_reflect,
      QC.testProperty "bsToInteger with show" prop_bsToInteger_show,
      QC.testProperty "calculate Sqrt with from Integer" prop_sqrt,
      QC.testProperty "calculate initial liquidity" prop_calculateInitialLiquidity,
      QC.testProperty "get amount out" prop_getAmountOut,
      QC.testProperty "get amount in" prop_getAmountIn
    ]

prop_integer_base256 :: Positive Integer -> Bool
prop_integer_base256 (Positive x) = base256ToInteger (integerToBase256 x) == x

prop_integer_bs_reflect :: Integer -> Bool
prop_integer_bs_reflect x = bsToInteger (integerToBS x) == x

prop_bsToInteger_show :: Integer -> Bool
prop_bsToInteger_show x = bsToInteger (stringToBuiltinByteString $ show x) == x

prop_sqrt :: Positive Integer -> Bool
prop_sqrt (Positive x) = calSqrt x == floor (sqrt @Double (fromIntegral x))

prop_calculateInitialLiquidity :: Positive Integer -> Positive Integer -> Bool
prop_calculateInitialLiquidity (Positive x) (Positive y) =
  let z = calculateInitialLiquidity x y
      z' = ceiling (sqrt @Double (fromIntegral (x * y)))
   in z == z'

prop_getAmountOut :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
prop_getAmountOut (Positive reserveA) (Positive reserveB) (Positive inA) =
  let outB = getAmountOut reserveA reserveB inA
   in checkSwap reserveA reserveB (reserveA + inA) (reserveB - outB)

prop_getAmountIn :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
prop_getAmountIn (Positive reserveA) (Positive reserveB) (Positive outB) =
  let inA = getAmountIn reserveA reserveB outB
   in case inA of
        Nothing -> outB >= reserveB
        Just inA' -> checkSwap reserveA reserveB (reserveA + inA') (reserveB - outB)

{-# INLINEABLE checkSwap #-}
checkSwap :: Integer -> Integer -> Integer -> Integer -> Bool
checkSwap oldA oldB newA newB =
  let inA = max 0 $ newA - oldA
      inB = max 0 $ newB - oldB
      feeNum, feeDen :: Integer
      feeNum = 3
      feeDen = 1000
   in (((newA * feeDen) - (inA * feeNum)) * ((newB * feeDen) - (inB * feeNum)))
        >= (feeDen * feeDen * oldA * oldB)
