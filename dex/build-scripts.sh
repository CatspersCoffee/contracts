#!/usr/bin/env bash

set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
cd "$here"

cardano-cli address build --payment-script-file plutus/batch_order_script.plutus --mainnet --out-file plutus/batch_order.addr
cardano-cli address build --payment-script-file plutus/pool_script.plutus --stake-verification-key-file keys/stake1.vkey --mainnet --out-file plutus/pool.addr
cardano-cli transaction policyid --script-file plutus/factory_minting_policy.plutus > plutus/factory_policyid
cardano-cli transaction policyid --script-file plutus/lp_minting_policy.plutus > plutus/lp_policyid
cardano-cli transaction policyid --script-file plutus/nft_minting_policy.plutus > plutus/nft_policyid
