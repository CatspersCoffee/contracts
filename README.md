# 😻 Minswap smart contracts 


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
