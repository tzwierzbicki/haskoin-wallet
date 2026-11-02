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

## The files of Bitcoin Core, in `core/`

Files Bitcoin Core 25.0 wrote on a regtest chain, for the specs that read
and sign the files of other wallets (`Tx/PsbtFileSpec.hs`,
`Database/PendingTxSpec.hs`, `Workflow/SigningSpec.hs`). Each is the base64
text Core's RPCs give, as a user would save it. The wallet `hw` of that node
holds the keys of the test account through the descriptors
`pkh([bc560f95/44h/0h/0h]tprv.../0/*)` and `.../1/*`, the account's key
`xprv9yHxeaL...` in the encoding of regtest, so that the keys and the
derivation records are those of the test wallet. A second wallet, `other`,
has keys of its own. Each wallet had blocks mined to a legacy address of its
own.

| File | How it was made |
| --- | --- |
| `core-prepared.psbt` | `walletcreatefundedpsbt` of the wallet `hw`: 1 BTC from the coin of 50 BTC mined to /0/0 of the test account, to a legacy address of `other`, with the change at /1/0 and `fee_rate` 10. The spec signs it with the test account's key into this file, and pins the result: `analyzepsbt` finds that result complete, `finalizepsbt` extracts the transaction, and `testmempoolaccept` accepts it on that chain. |
| `core-outside.psbt` | `walletcreatefundedpsbt` of the wallet `other`: 2.5 BTC of its own coin to another of its legacy addresses, `fee_rate` 10. It involves no account of the test wallet. |
| `hw-payment-partial.psbt` | `walletprocesspsbt` of the wallet `hw` on the file hw writes for the payment of `Tx/PsbtFileSpec.hs` (pinned there in base64), with `finalize` false: Core's signature as a partial signature, as a hardware wallet leaves it. |
| `hw-payment-finalized.psbt`, `.hex` | The same with `finalize` true, then `finalizepsbt`: the final script in place of the partial signature and the derivation records of the input, which Core clears, and the transaction alone. |
| `pending-payment-finalized.psbt`, `.hex` | The same for the payment of `Database/PendingTxSpec.hs` (8000 satoshis to the first address of another wallet, 11774 of change, at 1 satoshi per byte). |

The literal values the specs expect for these files, the scripts, the
derivation records, the fees and the addresses, are what `decodepsbt` shows
for them, with the addresses of regtest rewritten for mainnet, as hw shows
them on Bitcoin.
