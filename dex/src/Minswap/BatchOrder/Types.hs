{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Minswap.BatchOrder.Types
  ( OrderStep (..),
    OrderDatum (..),
    OrderRedeemer (..),
    POrderRedeemer (..),
    POrderDatum (..),
  )
where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic)
import Ledger (AssetClass, DatumHash)
import Ledger.Address (Address)
import Plutarch.Api.V1 (PDatumHash)
import Plutarch.Api.V1.Address (PAddress)
import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.DataRepr
  ( DerivePConstantViaData (DerivePConstantViaData),
    PDataFields,
    PIsDataReprInstances (PIsDataReprInstances),
    PLabeledType ((:=)),
  )
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)
import Plutarch.Prelude
import qualified PlutusTx
import PlutusTx.Prelude (Integer, Maybe)
import qualified Prelude as Haskell

data OrderStep
  = SwapExactIn
      { seiDesiredCoin :: AssetClass,
        seiMinimumReceive :: Integer
      }
  | SwapExactOut
      { seoDesiredCoin :: AssetClass,
        seoExpectedReceive :: Integer
      }
  | Deposit
      { dMinimumLP :: Integer
      }
  | Withdraw
      { wMinimumCoinA :: Integer,
        wMinimumCoinB :: Integer
      }
  | OneSideDeposit
      { osdDesiredCoin :: AssetClass,
        osdMinimumLP :: Integer
      }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed
  ''OrderStep
  [ ('SwapExactIn, 0),
    ('SwapExactOut, 1),
    ('Deposit, 2),
    ('Withdraw, 3),
    ('OneSideDeposit, 4)
  ]
PlutusTx.makeLift ''OrderStep

data OrderDatum = OrderDatum
  { odSender :: Address,
    odReceiver :: Address,
    odReceiverDatumHash :: Maybe DatumHash,
    odStep :: OrderStep,
    odBatcherFee :: Integer,
    odOutputADA :: Integer
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''OrderDatum [('OrderDatum, 0)]
PlutusTx.makeLift ''OrderDatum

data OrderRedeemer = ApplyOrder | CancelOrder

PlutusTx.makeIsDataIndexed
  ''OrderRedeemer
  [ ('ApplyOrder, 0),
    ('CancelOrder, 1)
  ]
PlutusTx.makeLift ''OrderRedeemer

-- | Plutarch synonym to 'OrderRedeemer'.
data POrderRedeemer (s :: S) = PApplyOrder (Term s (PDataRecord '[])) | PCancelOrder (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances POrderRedeemer)

instance PUnsafeLiftDecl POrderRedeemer where type PLifted POrderRedeemer = OrderRedeemer

deriving via (DerivePConstantViaData OrderRedeemer POrderRedeemer) instance (PConstant OrderRedeemer)

-- | Plutarch synonym to 'OrderDatum'.
newtype POrderDatum (s :: S)
  = POrderDatum
      ( Term
          s
          ( PDataRecord
              '[ "sender" ':= PAddress,
                 "receiver" ':= PAddress,
                 "receiverDatumHash" ':= PMaybeData PDatumHash,
                 -- TODO: This should ideally use the Plutarch synonym of 'POrderStep'.
                 "step" ':= PData,
                 "batcherFee" ':= PInteger,
                 "outputAda" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via (PIsDataReprInstances POrderDatum)

instance PUnsafeLiftDecl POrderDatum where type PLifted POrderDatum = OrderDatum

deriving via (DerivePConstantViaData OrderDatum POrderDatum) instance (PConstant OrderDatum)
