{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Minswap.Plutarch.Ledger (toValidatorHash, toPubKeyHash, ownCurrencySymbol) where

import Plutarch.Api.V1
  ( PAddress (..),
    PCredential (..),
    PCurrencySymbol,
    PScriptPurpose (..),
  )
import Plutarch.Prelude

-- | Similar to "Ledger.toValidatorHash".
toValidatorHash :: Term s (PAddress :--> PMaybe PByteString)
toValidatorHash = phoistAcyclic $
  plam $ \addr ->
    pfromData (pfield @"credential" # addr) `pmatch` \case
      PPubKeyCredential _ -> pcon PNothing
      PScriptCredential credFields -> pcon . PJust . pto . pfromData $ pfield @"_0" # credFields

-- | Similar to "Ledger.toPubKeyHash".
toPubKeyHash :: Term s (PAddress :--> PMaybe PByteString)
toPubKeyHash = phoistAcyclic $
  plam $ \addr ->
    pfromData (pfield @"credential" # addr) `pmatch` \case
      PPubKeyCredential credFields -> pcon . PJust . pto . pfromData $ pfield @"_0" # credFields
      PScriptCredential _ -> pcon PNothing

-- | Similar to "Ledger.ownCurrencySymbol" but operates on script purpose directly.
ownCurrencySymbol :: Term s (PScriptPurpose :--> PAsData PCurrencySymbol)
ownCurrencySymbol = phoistAcyclic $
  plam $
    flip pmatch $ \case
      PMinting te -> pfield @"_0" # te
      _ -> ptraceError "Plutarch.Ledger.ownCurrencySymbol: Incorrect script purpose"
