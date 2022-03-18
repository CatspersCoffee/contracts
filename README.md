# contracts
😻 Minswap smart contracts 

Build Guideline
- Making sure `nix` are available on your machine
- Checkout https://github.com/input-output-hk/plutus-apps to commit `5ffdb6362b9ba3e7095beccde56df0280abf12d0` 
- Run `nix-shell` command
- In `nix-shell` go to the `dex` folder
- Run `cabal run minswap-cli compile` and check your result with script in `plutus` folder
- Run `./build-scripts.sh` and check your result with script address and policyId in `plutus` folder
- Run `cabal run minswap-tests` to see internal testing result
- Run `cabal run tweag-audit` to see the audit test suites result

Audit report can be found here:
- Audit report on Jan 31: [Audit Report 1](https://github.com/minswap/contracts/tree/dex/dex/audit-report/MinSwap-Jan31.pdf)
- Audit report on March 03: [Audit Report 2](https://github.com/minswap/contracts/tree/dex/dex/audit-report/MinSwap-assignment-Mar-03.pdf)
