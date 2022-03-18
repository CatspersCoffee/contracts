{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Minswap.ConstantProductPool.Types
  ( PoolDatum (..),
    PoolRedeemer (..),
    PoolParams (..),
    Pool (..),
    ProfitSharing (..),
  )
where

import Plutus.V1.Ledger.Api (Address, CurrencySymbol, DatumHash)
import Plutus.V1.Ledger.Value (AssetClass, TokenName)
import qualified PlutusTx
import PlutusTx.Prelude (Eq, Integer, Maybe, (&&), (==), (||))
import qualified Prelude as Haskell

data ProfitSharing = ProfitSharing
  { psFeeTo :: Address,
    psFeeToDatumHash :: Maybe DatumHash
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''ProfitSharing [('ProfitSharing, 0)]
PlutusTx.makeLift ''ProfitSharing

instance Eq ProfitSharing where
  {-# INLINEABLE (==) #-}
  x == y =
    psFeeTo x == psFeeTo y
      && psFeeToDatumHash x == psFeeToDatumHash y

data PoolDatum = PoolDatum
  { pdCoinA :: AssetClass,
    pdCoinB :: AssetClass,
    pdTotalLiquidity :: Integer,
    pdRootKLast :: Integer,
    pdProfitSharing :: Maybe ProfitSharing
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''PoolDatum [('PoolDatum, 0)]
PlutusTx.makeLift ''PoolDatum

instance Eq PoolDatum where
  {-# INLINEABLE (==) #-}
  x == y =
    pdCoinA x == pdCoinA y
      && pdCoinB x == pdCoinB y
      && pdTotalLiquidity x == pdTotalLiquidity y
      && pdRootKLast x == pdRootKLast y
      && pdProfitSharing x == pdProfitSharing y

data PoolRedeemer
  = ApplyPool
      { apBatcherAddress :: Address,
        apLicenseIndex :: Integer
      }
  | UpdateFeeTo
      { uftOwnerIndex :: Integer
      }
  | WithdrawLiquidityShare
      { wlsOwnerIndex :: Integer,
        wlsFeeToIndex :: Integer
      }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed
  ''PoolRedeemer
  [('ApplyPool, 0), ('UpdateFeeTo, 1), ('WithdrawLiquidityShare, 2)]
PlutusTx.makeLift ''PoolRedeemer

data PoolParams = PoolParams
  { ppNftSymbol :: CurrencySymbol,
    ppLiquiditySymbol :: CurrencySymbol,
    ppFactoryCoin :: AssetClass,
    ppLicenseSymbol :: CurrencySymbol,
    ppOwnerTokenName :: TokenName
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''PoolParams [('PoolParams, 0)]
PlutusTx.makeLift ''PoolParams

data Pool = Pool
  { pCoinA :: AssetClass,
    pCoinB :: AssetClass
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''Pool [('Pool, 0)]
PlutusTx.makeLift ''Pool

instance Haskell.Eq Pool where
  {-# INLINEABLE (==) #-}
  x == y =
    (pCoinA x == pCoinA y && pCoinB x == pCoinB y)
      || (pCoinA x == pCoinB y && pCoinB x == pCoinA y)

instance Eq Pool where
  {-# INLINEABLE (==) #-}
  x == y =
    (pCoinA x == pCoinA y && pCoinB x == pCoinB y)
      || (pCoinA x == pCoinB y && pCoinB x == pCoinA y)
