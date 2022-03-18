
-- | This modules ensures that the liquidity that is accumulated once
-- profit sharing is turned on can be withdrawn.
module Audit.ConstantProductPool.WithdrawLiquidity where

import Cooked.MockChain

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Functor
import Control.Monad
import qualified Data.Map as M

import qualified Ledger.Value as Pl

import Audit.OffChain
import qualified Audit.ConstantProductPool.ProfitSharing as PS
import qualified Ledger.Typed.Scripts as Scripts
import Audit.MinSwapScripts

import Minswap.ConstantProductPool.Types
import Minswap.ConstantProductLiquidity.OnChain


tests :: TestTree
tests = testGroup "Withdraw Liquidity"
  [ testGroup "Unit tests"
    [ testProperty "can be withdrawn" $
        (testSucceedsFrom' (walletHasPositiveAsset wMinSwap) PS.i0 trCanWithdraw :: Property)
    ]
  ]

walletHasPositiveAsset :: (IsProp prop) => Wallet -> Pl.AssetClass -> UtxoState -> prop
walletHasPositiveAsset w ac (UtxoState st) =
  case M.lookup (walletAddress w) st of
    Nothing -> testFailureMsg "wallet has no assets at all"
    Just r  -> let totalR = utxoValueSetTotal r
               in testCounterexample "amount <= 0" $ testBool $ Pl.assetClassValueOf totalR ac > 0

-- Runs a trace that accrues some liquidityShares in the pool, then withdraws it
-- to wMinSwap's address.
trCanWithdraw :: (MonadMockChain m) => m Pl.AssetClass
trCanWithdraw = trCanWithdraw' poolValidator

trCanWithdraw' :: (MonadMockChain m) => Scripts.TypedValidator PoolValidator -> m Pl.AssetClass
trCanWithdraw' tgt = do
  (nftAC, _) <- txCreatePool pool `as` wallet 1
  void $ txUpdateFeeTo (feeUpd8 nftAC) `as` wMinSwap
  let liqAC = Pl.assetClass mkLiquiditySymbol (snd $ Pl.unAssetClass nftAC)
  void $ forM orders $ \gen
    -> let (order, orderW) = gen liqAC
       in do oid <- txCreateOrder order `as` orderW
             void $ txApplyOrders (TxApplyOrder nftAC [oid]) `as` wBatcher

  void $ txWithdrawLiquidityShare' tgt (TxWithdrawLiquidityShare nftAC) `as` wMinSwap
  -- We return the asset class of the pool's liquidity token, so we can check
  -- the payee really does receive it!
  return $ Pl.assetClass mkLiquiditySymbol (snd $ Pl.unAssetClass nftAC)
  where
    feeUpd8 ac = TxUpdateFeeTo ac (Just $ ProfitSharing (walletAddress wMinSwap) Nothing)

coinA, coinB :: Pl.AssetClass
coinA = quickAssetClass ""
coinB = quickAssetClass "YYY"

pool :: TxCreatePool
pool = TxCreatePool (coinA, 9748248) (coinB, 6565293)

orders :: [Pl.AssetClass -> (Order, Wallet)]
orders = [dep1, seo2, dep3, wit4]
  where
    dep1 _ = (DepositOrder $ TxCreateDepositOrder
               { cdoSelf = walletAddress $ wallet 3
               , cdoCoinA = (coinA,71063)
               , cdoCoinB = (coinB,35174)
               , cdoDepositLPMin = 0
               }
             , wallet 3
             )

    seo2 _ = (SwapExactOutOrder $ TxCreateSwapExactOutOrder
               { cseoSelf = walletAddress $ wallet 2
               , cseoDesiredCoin = coinA
               , cseoCoin = (coinB,69918)
               , cseoExpectedReceive = 102416
               }
             , wallet 2
             )

    dep3 _ = (DepositOrder $ TxCreateDepositOrder
               { cdoSelf = walletAddress $ wallet 2
               , cdoCoinA = (coinA,85420)
               , cdoCoinB = (coinB,91146)
               , cdoDepositLPMin = 0
               }
             , wallet 2
             )

    wit4 ac = (WithdrawOrder $ TxCreateWithdrawOrder
                 { cwoSelf = walletAddress $ wallet 1
                 , cwoWithdrawMin = (1,1)
                 , cwoWithdrawLP = Pl.assetClassValue ac 62224
                 }
              , wallet 1
              )
