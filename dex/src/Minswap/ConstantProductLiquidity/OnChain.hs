{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module Minswap.ConstantProductLiquidity.OnChain
  ( mkLiquidityScript,
    mkLiquiditySymbol,
    mkLiquidityPolicy,
  )
where

import Ledger (Script, scriptCurrencySymbol)
import Minswap.ConstantProductPoolNFT.OnChain
import qualified Minswap.Spooky.UntypedSpookyContexts as SC
import qualified Plutonomy
import Plutus.V1.Ledger.Api (CurrencySymbol, MintingPolicy, TokenName, Value (getValue), unMintingPolicyScript)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude

{-# INLINEABLE mkLiquidityPolicy #-}
mkLiquidityPolicy :: MintingPolicy
mkLiquidityPolicy =
  Plutonomy.optimizeUPLC $
    Plutonomy.mintingPolicyToPlutus originalLiquidityPolicy

originalLiquidityPolicy :: Plutonomy.MintingPolicy
originalLiquidityPolicy =
  Plutonomy.mkMintingPolicyScript
    ( $$(PlutusTx.compile [||mkLiquidityValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode mkNFTSymbol
    )

{-# INLINEABLE mkLiquidityScript #-}
mkLiquidityScript :: Script
mkLiquidityScript = unMintingPolicyScript mkLiquidityPolicy

{-# INLINEABLE mkLiquiditySymbol #-}
mkLiquiditySymbol :: CurrencySymbol
mkLiquiditySymbol = scriptCurrencySymbol mkLiquidityPolicy

-- | The 'mkLiquidityValidator' function validates the LP token is minted correctly
--
-- 1.   Validate that LP TokenName == NFT TokenName
{-# INLINEABLE mkLiquidityValidator #-}
mkLiquidityValidator :: CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkLiquidityValidator nftSymbol _ rawContext =
  let context = PlutusTx.unsafeFromBuiltinData rawContext
      info = SC.scriptContextTxInfo context
      ownSymbol = SC.ownCurrencySymbol context

      txOutputs :: [SC.TxOut]
      !txOutputs = SC.txInfoOutputs info

      mintValue :: Value
      !mintValue = SC.txInfoMint info

      nftTokenName :: TokenName
      nftTokenName = case [o | o <- txOutputs, isJust (SC.txOutDatumHash o)] of
        [o] -> case Map.lookup nftSymbol (getValue $ SC.txOutValue o) of
          Just i -> case [m | m@(_, am) <- Map.toList i, am == 1] of
            [(tn, _)] -> tn
            _ -> error ()
          _ -> error ()
        _ -> error ()

      lpTokenName :: TokenName
      lpTokenName = case Map.lookup ownSymbol (getValue mintValue) of
        Just i -> case Map.toList i of
          [(tn, _)] -> tn
          _ -> error ()
        _ -> error ()
   in if nftTokenName == lpTokenName -- 1.
        then ()
        else error ()
