{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- We suffix the record fields with ' to denote these are Spooky fields, and we provide accessor functions of the same name (wo the suffix) that unSpookify these fields.
module Minswap.Spooky.TypedSpookyContexts
  ( ScriptContext (..),
    scriptContextTxInfo,
    scriptContextPurpose,
    TxInfo (..),
    txInfoInputs,
    txInfoOutputs,
    txInfoFee,
    txInfoMint,
    txInfoDCert,
    txInfoWdrl,
    txInfoValidRange,
    txInfoSignatories,
    txInfoData,
    txInfoId,
    ScriptPurpose (..),
    ownCurrencySymbol,
    TxOut (..),
    txOutAddress,
    txOutValue,
    txOutDatumHash,
    TxInInfo (..),
    txInInfoOutRef,
    txInInfoResolved,
    spendsOutput,
    findDatum,
    findOwnInput,
  )
where

import GHC.Generics (Generic)
import Minswap.Spooky.Typed (Spooky, unSpooky)
import Plutus.V1.Ledger.Api
  ( Address,
    CurrencySymbol,
    DCert,
    Datum,
    DatumHash,
    POSIXTimeRange,
    PubKeyHash,
    StakingCredential,
    TxId,
    TxOutRef,
    Value,
    txOutRefId,
    txOutRefIdx,
  )
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Prelude
  ( Bool (False),
    Eq,
    Integer,
    Maybe (Nothing),
    any,
    error,
    find,
    snd,
    (&&),
    (.),
    (<$>),
    (==),
  )
import qualified Prelude as Haskell

data ScriptPurpose
  = Minting (Spooky CurrencySymbol)
  | Spending (Spooky TxOutRef)
  | Rewarding (Spooky StakingCredential)
  | Certifying (Spooky DCert)
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

data ScriptContext = ScriptContext
  { scriptContextTxInfo' :: Spooky TxInfo,
    scriptContextPurpose' :: Spooky ScriptPurpose
  }
  deriving stock (Generic, Haskell.Eq)

{-# INLINEABLE scriptContextTxInfo #-}
scriptContextTxInfo :: ScriptContext -> TxInfo
scriptContextTxInfo = unSpooky . scriptContextTxInfo'

{-# INLINEABLE scriptContextPurpose #-}
scriptContextPurpose :: ScriptContext -> ScriptPurpose
scriptContextPurpose = unSpooky . scriptContextPurpose'

{-# INLINEABLE ownCurrencySymbol #-}
ownCurrencySymbol :: ScriptContext -> CurrencySymbol
ownCurrencySymbol context =
  let purpose = scriptContextPurpose context
   in case purpose of
        Minting cs -> unSpooky cs
        _ -> error ()

data TxOut = TxOut
  { txOutAddress' :: Spooky Address,
    txOutValue' :: Spooky Value,
    txOutDatumHash' :: Spooky (Maybe DatumHash)
  }
  deriving stock (Haskell.Eq, Generic)

txOutAddress :: TxOut -> Address
txOutAddress = unSpooky . txOutAddress'

txOutValue :: TxOut -> Value
txOutValue = unSpooky . txOutValue'

txOutDatumHash :: TxOut -> Maybe DatumHash
txOutDatumHash = unSpooky . txOutDatumHash'

-- | An input of a pending transaction.
data TxInInfo = TxInInfo
  { txInInfoOutRef' :: Spooky TxOutRef,
    txInInfoResolved' :: Spooky TxOut
  }
  deriving stock (Generic, Haskell.Eq)

txInInfoOutRef :: TxInInfo -> TxOutRef
txInInfoOutRef = unSpooky . txInInfoOutRef'

txInInfoResolved :: TxInInfo -> TxOut
txInInfoResolved = unSpooky . txInInfoResolved'

-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
data TxInfo = TxInfo
  { -- | Transaction inputs
    txInfoInputs' :: Spooky [TxInInfo],
    -- | Transaction outputs
    txInfoOutputs' :: Spooky [TxOut],
    -- | The fee paid by this transaction.
    txInfoFee' :: Spooky Value,
    -- | The 'Value' minted by this transaction.
    txInfoMint' :: Spooky Value,
    -- | Digests of certificates included in this transaction
    txInfoDCert' :: Spooky [DCert],
    -- | Withdrawals
    txInfoWdrl' :: Spooky [(StakingCredential, Integer)],
    -- | The valid range for the transaction.
    txInfoValidRange' :: Spooky POSIXTimeRange,
    -- | Signatures provided with the transaction, attested that they all signed the tx
    txInfoSignatories' :: Spooky [PubKeyHash],
    txInfoData' :: Spooky [(DatumHash, Datum)],
    -- | Hash of the pending transaction (excluding witnesses)
    txInfoId' :: Spooky TxId
  }
  deriving stock (Generic, Haskell.Eq)

{-# INLINEABLE txInfoInputs #-}
txInfoInputs :: TxInfo -> [TxInInfo]
txInfoInputs = unSpooky . txInfoInputs'

{-# INLINEABLE txInfoOutputs #-}
txInfoOutputs :: TxInfo -> [TxOut]
txInfoOutputs = unSpooky . txInfoOutputs'

{-# INLINEABLE txInfoFee #-}
txInfoFee :: TxInfo -> Value
txInfoFee = unSpooky . txInfoFee'

{-# INLINEABLE txInfoMint #-}
txInfoMint :: TxInfo -> Value
txInfoMint = unSpooky . txInfoMint'

{-# INLINEABLE txInfoDCert #-}
txInfoDCert :: TxInfo -> [DCert]
txInfoDCert = unSpooky . txInfoDCert'

{-# INLINEABLE txInfoWdrl #-}
txInfoWdrl :: TxInfo -> [(StakingCredential, Integer)]
txInfoWdrl = unSpooky . txInfoWdrl'

{-# INLINEABLE txInfoValidRange #-}
txInfoValidRange :: TxInfo -> POSIXTimeRange
txInfoValidRange = unSpooky . txInfoValidRange'

{-# INLINEABLE txInfoSignatories #-}
txInfoSignatories :: TxInfo -> [PubKeyHash]
txInfoSignatories = unSpooky . txInfoSignatories'

{-# INLINEABLE txInfoData #-}
txInfoData :: TxInfo -> [(DatumHash, Datum)]
txInfoData = unSpooky . txInfoData'

{-# INLINEABLE txInfoId #-}
txInfoId :: TxInfo -> TxId
txInfoId = unSpooky . txInfoId'

{-# INLINEABLE spendsOutput #-}

-- | Check if the pending transaction spends a specific transaction output
--   (identified by the hash of a transaction and an index into that
--   transactions' outputs)
spendsOutput :: TxInfo -> TxId -> Integer -> Bool
spendsOutput p h i =
  let spendsOutRef inp =
        let outRef = txInInfoOutRef inp
         in h == txOutRefId outRef
              && i == txOutRefIdx outRef
   in any spendsOutRef (txInfoInputs p)

{-# INLINEABLE findDatum #-}

-- | Find the data corresponding to a data hash, if there is one
findDatum :: DatumHash -> TxInfo -> Maybe Datum
findDatum dsh info = snd <$> find f (txInfoData info)
  where
    f (dsh', _) = dsh' == dsh

{-# INLINEABLE findOwnInput #-}

-- | Find the input currently being validated.
findOwnInput :: ScriptContext -> Maybe TxInInfo
findOwnInput context =
  let info = scriptContextTxInfo context
      inputs = txInfoInputs info
   in case scriptContextPurpose context of
        Spending txOutRef -> find (\input -> txInInfoOutRef input == unSpooky txOutRef) inputs
        _ -> Nothing

instance Eq TxOut where
  {-# INLINEABLE (==) #-}
  TxOut a v dh == TxOut a' v' dh' =
    unSpooky a == unSpooky a'
      && unSpooky v == unSpooky v'
      && unSpooky dh == unSpooky dh'

instance Eq TxInInfo where
  TxInInfo ref res == TxInInfo ref' res' =
    unSpooky ref == unSpooky ref'
      && unSpooky res == unSpooky res'

instance Eq TxInfo where
  {-# INLINEABLE (==) #-}
  TxInfo i o f m c w r s d tid == TxInfo i' o' f' m' c' w' r' s' d' tid' =
    unSpooky i == unSpooky i'
      && unSpooky o == unSpooky o'
      && unSpooky f == unSpooky f'
      && unSpooky m == unSpooky m'
      && unSpooky c == unSpooky c'
      && unSpooky w == unSpooky w'
      && unSpooky r == unSpooky r'
      && unSpooky s == unSpooky s'
      && unSpooky d == unSpooky d'
      && unSpooky tid == unSpooky tid'

instance Eq ScriptPurpose where
  {-# INLINEABLE (==) #-}
  Minting cs == Minting cs' = unSpooky cs == unSpooky cs'
  Spending ref == Spending ref' = unSpooky ref == unSpooky ref'
  Rewarding sc == Rewarding sc' = unSpooky sc == unSpooky sc'
  Certifying cert == Certifying cert' = unSpooky cert == unSpooky cert'
  _ == _ = False

PlutusTx.makeIsDataIndexed ''TxOut [('TxOut, 0)]
PlutusTx.makeIsDataIndexed ''TxInInfo [('TxInInfo, 0)]

-- PlutusTx.makeLift ''TxInfo
PlutusTx.makeIsDataIndexed ''TxInfo [('TxInfo, 0)]

-- PlutusTx.makeLift ''SpookyScriptPurpose
PlutusTx.makeIsDataIndexed
  ''ScriptPurpose
  [ ('Minting, 0),
    ('Spending, 1),
    ('Rewarding, 2),
    ('Certifying, 3)
  ]

-- -- PlutusTx.makeLift ''SpookyScriptContext
PlutusTx.makeIsDataIndexed ''ScriptContext [('ScriptContext, 0)]
