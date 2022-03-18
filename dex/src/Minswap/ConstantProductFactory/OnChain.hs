{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module Minswap.ConstantProductFactory.OnChain
  ( mkFactoryCoin,
    mkFactoryScript,
    mkFactoryPolicy,
  )
where

import Data.Maybe (fromJust)
import Ledger (Script, scriptCurrencySymbol)
import Minswap.ConstantProductFactory.Types (FactoryParams (..))
import Minswap.ConstantProductLiquidity.OnChain (mkLiquiditySymbol)
import Minswap.ConstantProductPool.Types
  ( PoolDatum (pdCoinA, pdCoinB, pdProfitSharing, pdRootKLast, pdTotalLiquidity),
  )
import Minswap.ConstantProductPool.Utils (calculateInitialLiquidity, minimumLiquidity)
import Minswap.ConstantProductPoolNFT.OnChain (mkNFTSymbol)
import Minswap.ConstantProductPoolNFT.Utils (poolNFTOf)
import qualified Minswap.Spooky.TypedSpookyContexts as SC
import Minswap.Types.Coin (assetClass, assetClassValueOf, tokenNameOf)
import Minswap.Utils.OnChainUtils (assertPoolValue, mustFindScriptDatum')
import qualified Plutonomy
import Plutus.V1.Ledger.Api (CurrencySymbol, MintingPolicy, unMintingPolicyScript)
import Plutus.V1.Ledger.Value (AssetClass, TokenName, Value, tokenName)
import qualified PlutusTx
import PlutusTx.Prelude
  ( BuiltinData,
    error,
    isJust,
    isNothing,
    ($),
    (&&),
    (-),
    (<),
    (==),
    (>),
  )
import Text.Hex (decodeHex)

{-# INLINEABLE mkFactoryPolicy #-}
mkFactoryPolicy :: MintingPolicy
mkFactoryPolicy =
  Plutonomy.optimizeUPLC $
    Plutonomy.mintingPolicyToPlutus originalFactoryPolicy

{-# INLINEABLE mkFactoryScript #-}
mkFactoryScript :: Script
mkFactoryScript = unMintingPolicyScript mkFactoryPolicy

originalFactoryPolicy :: Plutonomy.MintingPolicy
originalFactoryPolicy =
  Plutonomy.mkMintingPolicyScript
    ( $$(PlutusTx.compile [||mkFactoryValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )
  where
    !params =
      FactoryParams
        { fpNftSymbol = mkNFTSymbol,
          fpLiquiditySymbol = mkLiquiditySymbol,
          fpFactoryTokenName = mkFactoryTokenName
        }

{-# INLINEABLE mkFactorySymbol #-}
mkFactorySymbol :: CurrencySymbol
mkFactorySymbol = scriptCurrencySymbol mkFactoryPolicy

{-# INLINEABLE mkFactoryTokenName #-}
mkFactoryTokenName :: TokenName
mkFactoryTokenName = tokenName $ fromJust $ decodeHex "4d494e53574150"

{-# INLINEABLE mkFactoryCoin #-}
mkFactoryCoin :: AssetClass
mkFactoryCoin = assetClass mkFactorySymbol mkFactoryTokenName

-- | The 'mkFactoryValidator' function validates the Liquidity Pool is created correctly
--
-- 1.   Coin A < Coin B
-- 2,3. Validate that LP token is minted correctly
-- 4,5. Validate that amount of token AB must be higher than zero
-- 6.   Validate that the pool value must contains necessary token inside
-- 7.   Validate that identified NFT token must be minted with quantity = 1
--      and is in Pool Value
-- 8.   Validate that Factory token must be minted with quantity = 1
-- 9.   Validate that Factory is in Pool Value
-- 10.  Validate that Profit Sharing must not be enable when Liquidity Pool is created
-- 11.  Validate that RootKLast must not be set when Liquidity Pool is created
{-# INLINEABLE mkFactoryValidator #-}
mkFactoryValidator :: FactoryParams -> BuiltinData -> BuiltinData -> ()
mkFactoryValidator FactoryParams {..} _ rawContext =
  if coinA < coinB -- 1.
    && actualLiquidity == expectedLiquidity -- 2.
    && actualTotalLiquidity == expectedTotalLiquidity -- 3.
    && amountA > 0 -- 4.
    && amountB > 0 -- 5.
    && validPoolValue --6.
    && assetClassValueOf forged nftCoinInOutput == 1 -- 7.
    && assetClassValueOf forged factoryCoin == 1 -- 8.
    && assetClassValueOf outVal factoryCoin == 1 -- 9.
    && isNothing (pdProfitSharing poolDatum) -- 10.
    && pdRootKLast poolDatum == 0 -- 11.
    then ()
    else error ()
  where
    context = PlutusTx.unsafeFromBuiltinData rawContext
    info = SC.scriptContextTxInfo context
    ownSymbol = SC.ownCurrencySymbol context

    forged :: Value
    !forged = SC.txInfoMint info

    txOutputs :: [SC.TxOut]
    !txOutputs = SC.txInfoOutputs info

    -- Create Pool transaction only allows a single script Output (Pool UTxO)
    -- We:
    --     - Filtering which TxOut has datum hash (line 117),
    --     - Making sure that there is only one script TxOut (line 121)
    --     - Making sure that this TxOut contain identified NFT token in the value (line 127)
    ownOutput :: SC.TxOut
    !ownOutput = case [ o
                        | o <- txOutputs,
                          isJust (SC.txOutDatumHash o)
                      ] of
      [o] -> o
      _ -> error ()

    !poolDatum = mustFindScriptDatum' @PoolDatum ownOutput info
    coinA = pdCoinA poolDatum
    coinB = pdCoinB poolDatum
    !outVal = SC.txOutValue ownOutput
    validPoolValue = assertPoolValue coinA coinB lpCoin outVal
    !nftCoinInOutput = poolNFTOf outVal fpNftSymbol
    !amountA = assetClassValueOf outVal coinA
    !amountB = assetClassValueOf outVal coinB
    actualTotalLiquidity = pdTotalLiquidity poolDatum
    !expectedTotalLiquidity = calculateInitialLiquidity amountA amountB

    actualLiquidity = assetClassValueOf forged lpCoin
    expectedLiquidity = expectedTotalLiquidity - minimumLiquidity

    !lpCoin = assetClass fpLiquiditySymbol (tokenNameOf nftCoinInOutput)
    !factoryCoin = assetClass ownSymbol fpFactoryTokenName
