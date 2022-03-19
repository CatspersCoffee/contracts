# 😻 Minswap smart contracts 

- Liquidity Pool Validator: [addr1z9tu3ecccgqlhgg2nkshfrt8td2zs8fmrwvrchgksl78x96j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq26n58l](https://cardanoscan.io/address/1157c8e718c201fba10a9da1748d675b54281d3b1b983c5d1687fc731752563c5410bff6a0d43ccebb7c37e1f69f5eb260552521adff33b9c2)
- Batch Order Validator: [addr1wyx22z2s4kasd3w976pnjf9xdty88epjqfvgkmfnscpd0rg3z8y6v](https://cardanoscan.io/address/710ca50950adbb06c5c5f6833924a66ac873e43202588b6d338602d78d)
- LP Token: [e0baa1f0887a766daf5196f92c88728e356e71255c5ad00866607484](https://cardanoscan.io/tokenPolicy/e0baa1f0887a766daf5196f92c88728e356e71255c5ad00866607484)
- Liquidity Pool Factory Token: [3f6092645942a54a75186b25e0975b79e1f50895ad958b42015eb6d2](https://cardanoscan.io/tokenPolicy/3f6092645942a54a75186b25e0975b79e1f50895ad958b42015eb6d2)
- Liquidity Pool NFT: [5178cc70a14405d3248e415d1a120c61d2aa74b4cee716d475b1495e](https://cardanoscan.io/tokenPolicy/5178cc70a14405d3248e415d1a120c61d2aa74b4cee716d475b1495e)

## Audit report

- Audit report on January 31st: [Audit Report](dex/audit-report/MinSwap-Jan31.pdf)
- Follow-up report on March 3rd: [Follow-up Report](dex/audit-report/MinSwap-assignment-Mar-03.pdf)

## Build Dapp on top of Minswap

Minswap supports smart contract composability by passing output script address and datum hash in receiver fields of BatchOrder datum. The BatchOrder's datum needs to be hashable to the same hash that cardano-cli produces. If you use [cardano-serialization-lib](https://github.com/Emurgo/cardano-serialization-lib) to build transactions, make sure to use the latest version as that is compatible with cardano-cli.

**IMPORTANT: BatchOrder's sender cannot be script address. If an order's sender is a script address, that order can never be cancelled.**

## Build steps

- Making sure `nix` are available on your machine
- Checkout https://github.com/input-output-hk/plutus-apps to commit `5ffdb6362b9ba3e7095beccde56df0280abf12d0` 
- Run `nix-shell` command
- In `nix-shell` go to the `dex` folder
- Run `cabal run minswap-cli compile` and check your result with script in `plutus` folder
- Run `./build-scripts.sh` and check your result with script address and policyId in `plutus` folder
- Run `cabal run minswap-tests` to see internal testing result
- Run `cabal run tweag-audit` to see the audit test suites result
