module Minswap.Plutarch.Utils (plookup, psingleMatch) where

import Plutarch.Api.V1 (PMap)
import Plutarch.Prelude

-- | Lookup the value associated with a key in an assoc map.
plookup :: Term s (PAsData k :--> PMap k v :--> PMaybe (PAsData v))
plookup = phoistAcyclic $
  plam $ \k m ->
    precList
      (\self x xs -> pif (pfstBuiltin # x #== k) (pcon . PJust $ psndBuiltin # x) (self # xs))
      (const $ pcon PNothing)
      # pto m

-- | This filters a builtin list with given predicate, ensures the result is a singleton list, and extracts the result.
--
-- It's like doing `isSingleton . filter` and then extracting `head`.
psingleMatch :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> PMaybe a)
psingleMatch = phoistAcyclic $
  plam $ \predc ->
    ( pfix #$ plam $ \self acc l ->
        pelimList
          ( \x xs -> plet (predc # x) $ \res ->
              pif
                res
                ( pmatch acc $ \case
                    PJust _ -> pnothing
                    PNothing -> self # pjust x # xs
                )
                $ self # acc # xs
          )
          acc
          l
    )
      # pnothing
  where
    pnothing = pcon PNothing
    pjust = pcon . PJust
