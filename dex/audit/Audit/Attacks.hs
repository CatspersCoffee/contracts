{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NumericUnderscores #-}

module Audit.Attacks where

import qualified Ledger.Ada as Pl
import qualified Ledger.Value as Pl
import qualified Ledger as Pl
import qualified PlutusTx.Numeric as Pl

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, Property)

import Minswap.BatchOrder.Types
import Minswap.ConstantProductPool.Types
import Minswap.ConstantProductLiquidity.OnChain (mkLiquiditySymbol)

import Audit.MinSwapScripts
import Audit.OffChain
import Audit.Attacks.DatumHijacking
import qualified Audit.ConstantProductPool.ProfitSharing as PS
import qualified Audit.ConstantProductPool.WithdrawLiquidity as WL
import Cooked.MockChain
import Cooked.Tx.Constraints

import Audit.ConstantProductPool.OrderGen

tests :: TestTree
tests = testGroup "Attacks" [
    -- documented as vuln:datum-hijacking on the report
    testCase "datum-hijacking on WithdrawLiquidity (!)" $
        testFailsFrom' isCekEvaluationFailure PS.i0 trDatumHijackAttack
    -- documented as vuln:order-stealing on the report
  , testCase "order-stealing on WithdrawLiquidity (!)" $
      testFailsFrom' isCekEvaluationFailure PS.i0 trOrderStealing
    -- documented as vuln:lp-token-dup on the report
  , testCase "mint LP tokens on WithdrawLiquidity (!)" $
      testFailsFrom' isCekEvaluationFailure PS.i0 trMintLPWithdraw

  , testProperty "mint LP tokens on ApplyOrder or on orders" propMintLPApplyOrder
  , testProperty "mint pool tokens on ApplyOrder or on orders" propMintPoolApplyOrder
  ]

attacker :: Wallet
attacker = wallet 6

-- * Datum Hijacking Attack
--
-- Relies on txWithdrawLiquidityShare; can be executed by arbitrary wallets.
trDatumHijackAttack :: (MonadMockChain m) => m ()
trDatumHijackAttack = void $ do
  void $ WL.trCanWithdraw' attackerScript

  -- After executing the attack, we'll transfer all the funds into the attacker's
  -- wallet just to prove it can be done. :)
  [dhOut] <- scriptUtxosSuchThat attackerScript (\_ _ -> True)
  attacker `signs` validateTxConstr
    -- any redeemer is fine here; the script ignores it anyway.
    [ SpendsScript attackerScript (WithdrawLiquidityShare 0 0) dhOut
    , paysPK (walletPKHash attacker) (sOutValue (fst dhOut))
    ]
  where
   attackerScript = datumHijacker $ StealerParams $ walletPKHash attacker

-- * Order Stealing
--
-- Relies on txWithdrawLiquidityShare to steal open orders.
trOrderStealing :: (MonadModalMockChain m) => m ()
trOrderStealing = void $ do
  -- For setting it up, create a pool with profit-sharing and execute some orders
  -- that will increate the amount liquidity shares in the pool.
  nftAC <- createAndExecute WL.pool True WL.orders

  -- Now, say there was an open order somewhere:
  void $ txCreateSwapExactInOrder
    (TxCreateSwapExactInOrder (walletAddress $ wallet 2) WL.coinB (WL.coinA, 123_456) 1)
      `as` wallet 2

  void $ txCreateSwapExactInOrder
    (TxCreateSwapExactInOrder (walletAddress $ wallet 2) WL.coinA (WL.coinB, 123_456) 1)
     `as` wallet 3

  -- An attacker can withdraw liquidity but use that very transaction to redeem the
  -- orders!
  orders <- scriptUtxosSuchThat orderValidator (\ _ _ -> True)
  everywhere (Just . steal orders) $
    txWithdrawLiquidityShare (TxWithdrawLiquidityShare nftAC) `as` attacker
  where
    steal orders (TxSkel lbl opts constr) =
      let tot = mconcat $ map (sOutValue . fst) orders
       in TxSkel lbl opts $ constr ++
            paysPK (walletPKHash attacker) tot
              : map (SpendsScript orderValidator ApplyOrder) orders

-- * Order Stealing
--
-- Relies on txWithdrawLiquidityShare to steal open orders.
trMintLPWithdraw :: (MonadModalMockChain m) => m ()
trMintLPWithdraw = void $ do
  nftAC <- createAndExecute WL.pool True WL.orders
  everywhere (Just . mintLP attacker nftAC) $
    txWithdrawLiquidityShare (TxWithdrawLiquidityShare nftAC) `as` attacker

propMintLPApplyOrder :: Property
propMintLPApplyOrder =
  forAll (commonParamsGen 10)
    (\(pool, orders, ps) -> testFailsFrom' isCekEvaluationFailure PS.i0 $ do
      nftAC <- createAndExecute pool ps []
      somewhere (Just . mintLP attacker nftAC) $
        runOrderGensInPool nftAC orders
    )

propMintPoolApplyOrder :: Property
propMintPoolApplyOrder =
  forAll (commonParamsGen 10)
    (\(pool, orders, ps) -> testFailsFrom' isCekEvaluationFailure PS.i0 $ do
      nftAC <- createAndExecute pool ps []
      let nftVal = Pl.assetClassValue nftAC 1
      somewhere (Just . mintPool nftVal) $
        runOrderGensInPool nftAC orders
    )
  where
    mintPool nftVal (TxSkel lbl opts constr) =
      TxSkel lbl opts $ constr <>
        [ mints [nftMP] nftVal
        , paysPK (walletPKHash attacker) (nftVal <> minAda)
        ]
    minAda = Pl.toValue (Pl.Lovelace 2_000_000)

mintLP :: Wallet -> Pl.AssetClass -> TxSkel -> TxSkel
mintLP eve nftAC (TxSkel lbl opts constr) =
    TxSkel lbl opts $ constr' ++
         [ mints [lpMP] liqVal
         , paysPK (walletPKHash eve) (liqVal <> minAda)
         ]
  where
    -- its important to deduct minAda from a PaysPK to the attacker if its already there, otherwise
    -- the transaction will be unbalanceable: there might be no other attacker utxo
    -- available to compensate the extra minLov to create the new attacker utxo.
    constr' = modPaysPK1 (walletPKHash attacker) (Pl.- minAda) constr
    minAda = Pl.toValue (Pl.Lovelace 2_000_000)
    liqAC = Pl.assetClass mkLiquiditySymbol (snd $ Pl.unAssetClass nftAC)
    liqVal = Pl.assetClassValue liqAC 42424242

modPaysPK1 :: Pl.PubKeyHash -> (Pl.Value -> Pl.Value) -> [Constraint] -> [Constraint]
modPaysPK1 _ _ [] = []
modPaysPK1 w f (PaysPKWithDatum pkh ms md v : rest)
  | pkh == w = PaysPKWithDatum pkh ms md (f v) : rest
  | otherwise = PaysPKWithDatum pkh ms md v : modPaysPK1 w f rest
modPaysPK1 w f (x : rest) = x : modPaysPK1 w f rest
