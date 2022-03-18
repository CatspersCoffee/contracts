{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Minswap.Types.Coin
  ( assetClassValue,
    unitValue,
    isUnity,
    assetClassValueOf,
    assetClass,
    adaCoin,
    symbolOf,
    tokenNameOf,
  )
where

import Ledger (AssetClass, CurrencySymbol, TokenName, Value)
import Ledger.Ada (adaSymbol, adaToken)
import Ledger.Value (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import PlutusTx.Prelude (Bool, (==))

{-# INLINEABLE unitValue #-}
unitValue :: AssetClass -> Value
unitValue c = assetClassValue c 1

{-# INLINEABLE isUnity #-}
isUnity :: Value -> AssetClass -> Bool
isUnity v c = assetClassValueOf v c == 1

{-# INLINEABLE adaCoin #-}
adaCoin :: AssetClass
adaCoin = assetClass adaSymbol adaToken

{-# INLINEABLE symbolOf #-}
symbolOf :: AssetClass -> CurrencySymbol
symbolOf (AssetClass (cs, _)) = cs

{-# INLINEABLE tokenNameOf #-}
tokenNameOf :: AssetClass -> TokenName
tokenNameOf (AssetClass (_, tn)) = tn
