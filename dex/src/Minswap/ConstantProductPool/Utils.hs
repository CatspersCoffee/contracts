{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Minswap.ConstantProductPool.Utils
  ( calculateInitialLiquidity,
    minimumLiquidity,
    getAmountOut,
    getAmountIn,
    calculateProfitSharing,
    calSqrt,
    calculateSwapAmount,
    calculateDepositAmount,
  )
where

import PlutusTx.Prelude

{-# INLINEABLE minimumLiquidity #-}
minimumLiquidity :: Integer
minimumLiquidity = 10

{-# INLINEABLE calSqrt #-}
calSqrt :: Integer -> Integer
calSqrt x
  | x < 0 = traceError ""
  | x == 0 = 0
  | x == 1 = 1
  | x == 2 = 1
  | otherwise = go x (x `divide` 2 + 1)
  where
    go :: Integer -> Integer -> Integer
    go i1 i2 =
      if i2 < i1
        then go i2 ((x `divide` i2 + i2) `divide` 2)
        else i1

{-# INLINEABLE calculateInitialLiquidity #-}
calculateInitialLiquidity :: Integer -> Integer -> Integer
calculateInitialLiquidity outA outB =
  let p = outA * outB
      sqrt = calSqrt p
   in if sqrt * sqrt < p
        then sqrt + 1
        else sqrt

{-# INLINEABLE getAmountOut #-}
getAmountOut :: Integer -> Integer -> Integer -> Integer
getAmountOut reserveA reserveB inA =
  let inAWithFee = inA * 997
      numerator = inAWithFee * reserveB
      denominator = reserveA * 1000 + inAWithFee
      outB = numerator `divide` denominator
   in outB

{-# INLINEABLE getAmountIn #-}
getAmountIn :: Integer -> Integer -> Integer -> Maybe Integer
getAmountIn reserveA reserveB outB =
  if outB >= reserveB
    then Nothing
    else
      let numerator = reserveA * outB * 1000
          denominator = (reserveB - outB) * 997
          inA = numerator `divide` denominator
       in Just $ inA + 1

-- Calculate liquidity amount which will be minted for profit sharing
-- The protocol will collect 0.05% of trading fee
-- Refer: https://uniswap.org/whitepaper.pdf (2.4 Protocol fee)
{-# INLINEABLE calculateProfitSharing #-}
calculateProfitSharing ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer
calculateProfitSharing rootKLast reserveA reserveB totalLiquidity =
  if rootKLast <= 0
    then 0
    else
      let rootK = calSqrt (reserveA * reserveB)
       in if rootK > rootKLast
            then
              let numerator = totalLiquidity * (rootK - rootKLast)
                  denominator = (rootK * 5) + rootKLast
                  liquidity = numerator `divide` denominator
               in if liquidity > 0 then liquidity else 0
            else 0

-- Calculate which amount will be swapped to supply enough amount for depositing
-- Refer: https://blog.alphafinance.io/onesideduniswap/
{-# INLINEABLE calculateSwapAmount #-}
calculateSwapAmount ::
  Integer ->
  Integer ->
  Integer
calculateSwapAmount reserve delta =
  let reserve' = 1997 * reserve
      numerator =
        calSqrt
          (reserve' * reserve' + 4 * 997 * 1000 * delta * reserve)
          - (1997 * reserve)
      denominator = 2 * 997
   in numerator `divide` denominator

{-# INLINEABLE calculateDepositAmount #-}
calculateDepositAmount ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Maybe (Integer, Integer, Integer)
calculateDepositAmount
  deltaA
  deltaB
  amountA
  amountB
  totalLiquidity =
    let deltaLiquidityA = (deltaA * totalLiquidity) `divide` amountA
        deltaLiquidityB = (deltaB * totalLiquidity) `divide` amountB
        (deltaA', deltaB')
          | deltaLiquidityA > deltaLiquidityB = ((deltaB * amountA) `divide` amountB, deltaB)
          | deltaLiquidityA < deltaLiquidityB = (deltaA, (deltaA * amountB) `divide` amountA)
          | otherwise = (deltaA, deltaB)
        deltaLiquidity = min deltaLiquidityA deltaLiquidityB
     in if deltaA' > 0 && deltaB' > 0 && deltaLiquidity > 0
          then Just (deltaA', deltaB', deltaLiquidity)
          else Nothing
