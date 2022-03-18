{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

-- | Here you will find all the MinSwap scripts wrapped into 'Pl.TypedValidator's
-- to be used with /cooked-validators/.
module Audit.MinSwapScripts where

import qualified Ledger.Scripts as Pl
import qualified Ledger.Typed.Scripts as Pl

import Minswap.BatchOrder.Types
import Minswap.BatchOrder.OnChain
import Minswap.ConstantProductFactory.OnChain
import Minswap.ConstantProductPool.OnChain
import Minswap.ConstantProductPool.Types
import Minswap.ConstantProductPoolNFT.OnChain
import Minswap.ConstantProductLiquidity.OnChain
import Unsafe.Coerce (unsafeCoerce)

nftMP :: Pl.MintingPolicy
nftMP = mkNFTPolicy

factoryMP :: Pl.MintingPolicy
factoryMP = mkFactoryPolicy

lpMP :: Pl.MintingPolicy
lpMP = mkLiquidityPolicy

data BatchOrderValidator
instance Pl.ValidatorTypes BatchOrderValidator where
  type RedeemerType BatchOrderValidator = OrderRedeemer
  type DatumType BatchOrderValidator = OrderDatum

orderValidator :: Pl.TypedValidator BatchOrderValidator
orderValidator = unsafeCoerce $ Pl.unsafeMkTypedValidator $ Pl.Validator mkBatchOrderScript

data PoolValidator
instance Pl.ValidatorTypes PoolValidator where
  type RedeemerType PoolValidator = PoolRedeemer
  type DatumType PoolValidator = PoolDatum

poolValidator :: Pl.TypedValidator PoolValidator
poolValidator = unsafeCoerce $ Pl.unsafeMkTypedValidator $ Pl.Validator mkPoolScript