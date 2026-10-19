# Sweep signing test vectors

Test vectors for `signsweep` (`signTxWithKeys` in `Haskoin.Wallet.TxSweep`):
coins of external private keys, swept with those keys. They are loaded by
`test/Haskoin/Wallet/TxSweepSpec.hs`. Every `.json` file in this directory is
tested, so new vectors can be added without changing the test code.

The format is the one of [../tx-signing](../tx-signing/README.md). Every
vector has `sweepKeys`; its `account` only identifies the wallet's own inputs
and outputs, if any.

## Files

| File | Origin |
| --- | --- |
| `trezor.json` | Trezor firmware device tests: SegWit coins of the Trezor test mnemonic |
| `external.json` | Ledger device tests (SegWit coins), bitcoinjs-lib and Electrum tests (P2PKH coins with WIF keys) |
| `hw-regression.json` | Produced by hw itself (regression vectors) |
| `edge-cases.json` | Sweeps that hw must refuse to sign, with the exact error |

Verify the vectors with the tools of the signing vectors:

```sh
cd ../tx-signing/tools && python3 verify_vectors.py ../../tx-sweep
```
