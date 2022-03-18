{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Api hiding (Script)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Minswap.BatchOrder.OnChain
import Minswap.ConstantProductFactory.OnChain
import Minswap.ConstantProductLiquidity.OnChain
import Minswap.ConstantProductPool.OnChain
import Minswap.ConstantProductPoolNFT.OnChain
import Plutus.V1.Ledger.Scripts (Script, scriptSize)
import qualified Plutus.V1.Ledger.Api as Plutus
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "compile" -> do
      writePlutusScript' "NFT minting" "plutus/nft_minting_policy.plutus" mkNFTScript
      writePlutusScript' "LP minting" "plutus/lp_minting_policy.plutus" mkLiquidityScript
      writePlutusScript' "Factory minting" "plutus/factory_minting_policy.plutus" mkFactoryScript
      writePlutusScript' "Pool" "plutus/pool_script.plutus" mkPoolScript
      writePlutusScript' "Batch order" "plutus/batch_order_script.plutus" mkBatchOrderScript
    _ -> error "Command not supported"

writePlutusScript' :: String -> FilePath -> Script -> IO ()
writePlutusScript' title filename scrpt =
  do
    let scriptSBS = SBS.toShort . LBS.toStrict . serialise $ scrpt
    let scriptSerial = PlutusScriptSerialised scriptSBS :: PlutusScript PlutusScriptV1
    case Plutus.defaultCostModelParams of
      Just m ->
        let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS []
         in do
              print title
              print ("Log output" :: String) >> print logout
              case e of
                Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                Right exbudget -> do
                  print ("Ex Budget" :: String) >> print exbudget
                  print ("Script size " :: String) >> print (scriptSize scrpt)
      Nothing -> error "defaultCostModelParams failed"
    result <- writeFileTextEnvelope filename Nothing scriptSerial
    case result of
      Left err -> print $ displayError err
      Right () -> return ()
