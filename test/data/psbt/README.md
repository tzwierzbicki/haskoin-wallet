# PSBT test vectors

`bip174.json` holds the test vectors of [BIP 174](https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki),
copied from the BIP (BSD-2-Clause), for `test/Haskoin/Wallet/Bip174Spec.hs`:

| Key | What it holds |
| --- | --- |
| `invalid` | The invalid PSBTs, each with the `case` the BIP gives, in `hex` and in `base64`. hw must refuse every one. |
| `valid` | The valid PSBTs, the same way. hw must read every one and write it back byte for byte. |
| `roles` | The PSBTs of the walk-through of the roles, in the order of the BIP: creator, updater, updater with SIGHASH_ALL, the two signers, combiner, finalizer, the two PSBTs with unknown keys and their combination, and the two PSBTs to combine and their combination. In `hex` only. |
| `extracted` | The transaction the extractor gives for the finalizer's PSBT. |

The literal values the specs expect for the fields of these vectors (keys,
fingerprints, paths, scripts) are what Bitcoin Core 25.0 shows for them with
`decodepsbt`.

## The file of hw

`test/Haskoin/Wallet/Tx/PsbtFileSpec.hs` pins the PSBT hw writes for the
payment of the test wallet, in base64. It was produced by hw and checked with
Bitcoin Core's `decodepsbt` (the unsigned transaction, the previous
transaction of the input, the derivation records with the fingerprint
`bc560f95` and the paths `m/44'/0'/0'/0/0` and `m/44'/0'/0'/1/0`, the sighash
type, the account's xpub, and hw's proprietary fields under the identifier
`6877`, "hw"). To check it again, run a regtest node and decode the string:

```sh
bitcoind -regtest -datadir=/tmp/hw-regtest -daemon -connect=0 -listen=0
bitcoin-cli -regtest -datadir=/tmp/hw-regtest decodepsbt BASE64
```

The public keys of the test wallet the specs name were derived apart from hw
with the Python tool of the message-signing vectors
(`../message-signing/tools/btc.py`).
