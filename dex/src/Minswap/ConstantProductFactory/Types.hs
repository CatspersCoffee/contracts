{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Minswap.ConstantProductFactory.Types
  ( FactoryParams (..),
  )
where

import Plutus.V1.Ledger.Api (CurrencySymbol, TokenName)
import qualified PlutusTx
import qualified Prelude as Haskell

data FactoryParams = FactoryParams
  { fpNftSymbol :: CurrencySymbol,
    fpLiquiditySymbol :: CurrencySymbol,
    fpFactoryTokenName :: TokenName
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''FactoryParams [('FactoryParams, 0)]
PlutusTx.makeLift ''FactoryParams
