{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Minswap.Utils.OnChainUtils
  ( assertPoolValue,
    mustFindScriptDatum,
    mustFindScriptDatum',
    integerToBS,
    bsToInteger,
    encodeHex,
    integerToBase256,
    base256ToInteger,
  )
where

import qualified Minswap.Spooky.TypedSpookyContexts as SC
import Minswap.Types.Coin (adaCoin, assetClassValueOf)
import Plutus.V1.Ledger.Api (Datum (Datum))
import Plutus.V1.Ledger.Contexts (TxInfo, TxOut, findDatum)
import Plutus.V1.Ledger.Tx (txOutDatum)
import Plutus.V1.Ledger.Value (AssetClass, Value, flattenValue)
import qualified PlutusTx
import PlutusTx.IsData.Class (UnsafeFromData)
import PlutusTx.Prelude

minusAsciiCode :: Integer
minusAsciiCode = 45

zeroAsciiCode :: Integer
zeroAsciiCode = 48

{-# INLINEABLE assertPoolValue #-}
assertPoolValue :: AssetClass -> AssetClass -> AssetClass -> Value -> Bool
assertPoolValue coinA coinB lpCoin v =
  let valueLength = length $ flattenValue v
      hasLPCoin = assetClassValueOf v lpCoin > 0
      hasOneSideADA = coinA == adaCoin || coinB == adaCoin
   in if hasLPCoin && hasOneSideADA
        then valueLength == 5
        else
          if not hasLPCoin && hasOneSideADA
            then valueLength == 4
            else
              if hasLPCoin && not hasOneSideADA
                then valueLength == 6
                else valueLength == 5

{-# INLINEABLE mustFindScriptDatum #-}
mustFindScriptDatum :: (UnsafeFromData d) => TxOut -> TxInfo -> d
mustFindScriptDatum o info = case txOutDatum o of
  Just dh -> case findDatum dh info of
    Just (Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
    _ -> error ()
  _ -> error ()

{-# INLINEABLE mustFindScriptDatum' #-}
mustFindScriptDatum' :: (UnsafeFromData d) => SC.TxOut -> SC.TxInfo -> d
mustFindScriptDatum' o info = case SC.txOutDatumHash o of
  Just dh -> case SC.findDatum dh info of
    Just (Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
    _ -> error ()
  _ -> error ()

-- Convert from an integer to its text representation. Example: 123 => "123"
{-# INLINEABLE integerToBS #-}
integerToBS :: Integer -> BuiltinByteString
integerToBS x
  | x < 0 = consByteString minusAsciiCode $ integerToBS (negate x)
  -- x is single-digit
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = integerToBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    digitToBS d = consByteString (d + zeroAsciiCode) emptyByteString

-- Convert an ASCII string representing an integer to an integer. Error if invalid.
{-# INLINEABLE bsToInteger #-}
bsToInteger :: BuiltinByteString -> Integer
bsToInteger input = go 0 0
  where
    len = lengthOfByteString input

    go :: Integer -> Integer -> Integer
    go idx acc
      | idx == len = acc
      | idx == 0 && byte == minusAsciiCode = negate $ go (idx + 1) acc
      | byte < zeroAsciiCode || byte > zeroAsciiCode + 9 = error ()
      | otherwise = go (idx + 1) (acc * 10 + (byte - zeroAsciiCode))
      where
        byte = indexByteString input idx

-- Convert from a byte string to its hex (base16) representation. Example: [2, 14, 255] => "020eff"
{-# INLINEABLE encodeHex #-}
encodeHex :: BuiltinByteString -> BuiltinByteString
encodeHex input = go 0
  where
    len = lengthOfByteString input

    go :: Integer -> BuiltinByteString
    go i
      | i == len = emptyByteString
      | otherwise =
        consByteString (toChar $ byte `quotient` 16) $
          consByteString (toChar $ byte `remainder` 16) (go $ i + 1)
      where
        byte = indexByteString input i

        toChar :: Integer -> Integer
        toChar x
          -- 48 is ASCII code for '0'
          | x < 10 = x + 48
          -- 97 is ASCII code for 'a'
          -- x - 10 + 97 = x + 87
          | otherwise = x + 87

-- Only works with positive integer
{-# INLINEABLE integerToBase256 #-}
integerToBase256 :: Integer -> BuiltinByteString
integerToBase256 x
  | x < 0 = error ()
  | x < 256 = toByte x
  | otherwise = integerToBase256 (x `quotient` 256) <> toByte (x `remainder` 256)
  where
    toByte :: Integer -> BuiltinByteString
    toByte y = consByteString y emptyByteString

-- Only works with positive integer
{-# INLINEABLE base256ToInteger #-}
base256ToInteger :: BuiltinByteString -> Integer
base256ToInteger input = go 0 0
  where
    len = lengthOfByteString input

    go :: Integer -> Integer -> Integer
    go idx acc
      | idx == len = acc
      | otherwise = go (idx + 1) (acc * 256 + byte)
      where
        byte = indexByteString input idx
