{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Minswap.BatchOrder.OnChain (mkBatchOrderScript) where

import Ledger (Script, Validator, ValidatorHash (..), unValidatorScript, validatorHash)
import Minswap.BatchOrder.Types (POrderDatum, POrderRedeemer (..))
import Minswap.ConstantProductPool.OnChain (mkPoolScript)
import qualified Minswap.Plutarch.Ledger as PLedger
import Minswap.Plutarch.Utils (psingleMatch)
import Minswap.Plutarch.Utils.Cont (pmatchC, ptraceC)
import Plutarch (compile)
import Plutarch.Api.V1 (PAddress, PPubKeyHash (PPubKeyHash), PScriptContext)
import Plutarch.Prelude
import qualified Plutonomy
import Plutus.V1.Ledger.Api (fromBuiltin)
import Plutus.V1.Ledger.Scripts (Validator (..))

poolScriptHash :: Term s PByteString
poolScriptHash =
  let (Ledger.ValidatorHash vh) = validatorHash $ Validator $ mkPoolScript
   in pconstant . fromBuiltin $ vh

batchOrderScript :: Script
batchOrderScript = compile $ batchOrderValidator # poolScriptHash

mkBatchOrderPlutonomyValidator :: Validator
mkBatchOrderPlutonomyValidator = Validator batchOrderScript

mkBatchOrderScript :: Script
mkBatchOrderScript = unValidatorScript $ Plutonomy.optimizeUPLC mkBatchOrderPlutonomyValidator

batchOrderValidator ::
  forall s.
  Term
    s
    ( PByteString
        :--> POrderDatum
        :--> POrderRedeemer
        :--> PScriptContext
        :--> PUnit
    )
batchOrderValidator = plam $ \ph pdatm predm pctx -> unTermCont $ do
  -- Extract the txInfo field and bind the necessary fields to it - inputs and signatories.
  info <- tcont . pletFields @'["inputs", "signatories"] . pfromData $ pfield @"txInfo" # pctx
  redm <- pmatchC predm
  case redm of
    PApplyOrder _ -> do
      ptraceC "ApplyOrder case"
      let txInputs = pfromData $ hrecField @"inputs" info

          isPoolAddress :: Term s (PAddress :--> PBool)
          isPoolAddress = plam $ \addr ->
            pmatch (PLedger.toValidatorHash # addr) $ \case
              PJust hash -> ptraceIfTrue "Found matching hash!" $ hash #== ph
              PNothing -> pcon PFalse
      -- Filter txInputs with isPoolAddress, applied over the "address" field of "txInfoResolved".
      -- The result must produce a singular match.
      PJust _ <-
        pmatchC $
          psingleMatch
            # plam
              ( \x ->
                  let resolved = pfromData $ pfield @"resolved" # pfromData x
                   in isPoolAddress #$ pfromData $ pfield @"address" # resolved
              )
            # txInputs
      pure $ pconstant ()
    PCancelOrder _ -> do
      ptraceC "CancelOrder case"
      let sender = pfromData $ pfield @"sender" # pdatm
          signatories = pfromData $ hrecField @"signatories" info

      -- The sender field in datum should have a pub key hash.
      PJust ownerPubKeyHash <- pmatchC $ PLedger.toPubKeyHash # sender
      pure $
        pif
          (pelem # pdata (pcon $ PPubKeyHash ownerPubKeyHash) # signatories)
          (ptrace "Found pub key in signatories" $ pconstant ())
          $ ptraceError "Pub key missing from signatories"
