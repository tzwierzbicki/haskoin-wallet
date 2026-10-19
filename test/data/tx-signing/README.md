# Transaction signing test vectors

Test vectors for `signtx` (`signWalletTx` in `Haskoin.Wallet.TxSigning`). They
are loaded by `test/Haskoin/Wallet/TxSigningSpec.hs`. Every `.json` file in this
directory is tested, so new vectors can be added without changing the test code.
Sweep vectors, signed with external keys, are in [../tx-sweep](../tx-sweep/README.md)
in the same format.

## Files

| File | Origin |
| --- | --- |
| `trezor.json` | Trezor firmware device tests: Bitcoin, Testnet and Bitcoin Cash transactions signed by a Trezor, which hw must reproduce byte for byte |
| `external.json` | A Ledger device test: a P2PKH transaction signed by a Ledger, which hw must reproduce byte for byte |
| `hw-regression.json` | Produced by hw itself (regression vectors) |
| `edge-cases.json` | Transactions that hw must refuse to sign, with the exact error |

External vectors were extracted from the upstream sources at the commits given
in each `sourceUrl`. All signed transactions were independently verified with
the pure-Python script in [tools/](tools/README.md).

## Format

Each file is a JSON object with a `description` and a list of `vectors`:

| Field | Description |
| --- | --- |
| `name` | Unique name, prefixed by the origin |
| `network` | haskoin network name: `btc`, `btctest`, `bch`, `bchtest` |
| `account` | The wallet account: `{"mnemonic", "passphrase", "account"}` (passphrase optional). Its key `m/44'/coin'/account'` signs the transaction, or only identifies the wallet's own inputs and outputs when `sweepKeys` is given |
| `sweepKeys` | Only in `../tx-sweep`. Private keys of a sweep transaction, in the key format of the message signing vectors (`wif`, `secretHex`, or `mnemonic` with `path`). The transaction is then signed like `signsweep` does |
| `txSignData` | The transaction to sign, in hw's own TxSignData format: unsigned `tx` (hex), the previous transactions `txinputs` (hex), and the `inputpaths` and `outputpaths` of the wallet's own inputs and outputs, relative to the account (`/chain/index`) |
| `signed` | What hw must produce: the signed `tx` (hex), its `txid`, and `txInfo`, the `type` (`debit`, `credit` or `internal`), `amount`, `fee`, `size` (of the signed transaction, in bytes) and `feeByte` reported with it |
| `expect` | `match` (default): hw must produce `signed`. `error`: hw must refuse to sign with the message in `error`. A vector with both `expect: error` and `signed` documents a known bug: the comment says what to do once it is fixed |
| `source`, `sourceUrl`, `comment` | Provenance and explanations |
