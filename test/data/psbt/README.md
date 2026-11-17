# PSBT test vectors

This folder holds the vectors of hw's own transaction files. The PSBT format
itself, `Haskoin.Transaction.Psbt`, lives in haskoin-core, with the test
vectors of [BIP 174](https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki)
(`data/psbt_bip174.json` there) and the spec that checks them.

## The file of hw

`test/Haskoin/Wallet/Tx/PsbtFileSpec.hs` pins the PSBT hw writes for the
payment of the test wallet, in base64. hw produced it, and Bitcoin Core's
`decodepsbt` confirmed what it holds: the unsigned transaction, the previous
transaction of the input, the derivation records with the fingerprint
`bc560f95` and the paths `m/44'/0'/0'/0/0` and `m/44'/0'/0'/1/0`, the sighash
type, and hw's proprietary fields under the identifier `6877`, "hw". To
check it again, run a regtest node and decode the string:

```sh
bitcoind -regtest -datadir=/tmp/hw-regtest -daemon -connect=0 -listen=0
bitcoin-cli -regtest -datadir=/tmp/hw-regtest decodepsbt BASE64
```

The specs name public keys of the test wallet. They were derived
independently of hw, with the Python tool of the message-signing vectors
(`../message-signing/tools/btc.py`).

`segwitPaymentText` in `test/Haskoin/Wallet/Tx/PsbtFileSpec.hs` pins a second
file: the one hw writes for the same payment made by the bip84 account of the
test wallet. `decodepsbt` confirmed it the same way: the coin of its input,
both as its whole transaction and as itself (`witness_utxo`), the derivation
records `m/84'/0'/0'/0/0` and `m/84'/0'/0'/1/0`, and hw's fields. Its public
keys were derived independently of hw the same way.

Both pinned files carry the path of the address that their input spends
from, `/0/0`. The path is in hw's field of subtype 2 of the input: the key
`fc02687702` and the value `0000000000000000`, which is the chain and the
index, 4 bytes each, little-endian. The files were first checked without
that field. A script then added the field, independently of hw. The script
was written from BIP 174, and rewrites the map of the input key by key.
`decodepsbt` of Bitcoin Core 32.1 shows the field among the proprietary
fields of the input, and the rest of each file unchanged. `analyzepsbt`
names the signer as the next role.

## The files of Bitcoin Core, in `core/`

Bitcoin Core wrote these files on a regtest chain. The specs that read and
sign the files of other wallets use them (`Tx/PsbtFileSpec.hs`,
`Database/PendingTxSpec.hs`, `Workflow/SigningSpec.hs`). Each file is the
base64 text that Core's RPCs give, as a user would save it.

Core 25.0 wrote them, except the files that the text below gives to Core
32.1. Core 32.1 writes the same bytes, signs and finalizes the same way, and
accepts the same transaction, when `walletcreatefundedpsbt` is asked for a
PSBT of version 0 with its `psbt_version` argument. Since Core 32.0, its
default is version 2 (BIP 370), which hw reads too. The files named `-v2`
are of version 2.

The node has three wallets:

- `hw` holds the keys of the test account, through the descriptors
  `pkh([bc560f95/44h/0h/0h]tprv.../0/*)` and `.../1/*`. Their key is the
  account's key `xprv9yHxeaL...` in the encoding of regtest. So the keys and
  the derivation records of the files are those of the test wallet.
- `other` has keys of its own: the descriptors
  `pkh(tprv8ZgxMBicQKsPdLA1DWtYbSdVVfjzd7dNqEvvBFL6V6YjwLkiQnzT6y9NgSHFG27K3sKpofaeeZjrSZkomqA7YRtncLyEEnuqhRWUeMMHq3T/44'/1'/0'/0/*)`
  and `.../1/*`, with the fingerprint `b7253d71`.
- `hw84` holds the keys of the bip84 account of the test wallet the same
  way, through `wpkh([bc560f95/84h/0h/0h]tprv.../0/*)` and `.../1/*`. Their
  key is `xprv9yQtifF...` in the encoding of regtest
  (`tprv8g5qVzZb7thKDKxbZnD4BBfZjesetceTNJkuNS6CwvsDzwTYAd8awzifSNCxyNuZngEcn5BqS9MhuD6Eiz9oqLVDDz3bvvSWDzNkB4MHYLB`).
  The `-segwit` files come from this wallet. Core 32.1 wrote them.

The coins come from two chains. On the first chain, `other` mined blocks 1
to 101 to its first address, then `hw` mined 101 blocks to its address /0/0.
So the coin of block 102 is the one mature coin of `hw`. `hw84` mined 101
blocks to its address /0/0 on a chain of its own, so the coin of block 1 is
its one mature coin. `other` holds a legacy address on that chain too.

The change of each file is at the first change address of its wallet. A
wallet that made a file before gives that address again with the
`changeAddress` option.

| File | How it was made |
| --- | --- |
| `core-prepared.psbt` | `walletcreatefundedpsbt` of the wallet `hw`: 1 BTC from the coin of 50 BTC mined to /0/0 of the test account, to a legacy address of `other`, with the change at /1/0 and `fee_rate` 10. The spec signs this file with the test account's key, and pins the result: `analyzepsbt` finds that result complete, `finalizepsbt` extracts the transaction, and `testmempoolaccept` accepts it on that chain. |
| `core-outside.psbt` | `walletcreatefundedpsbt` of the wallet `other`: 2.5 BTC of its own coin to another of its legacy addresses, `fee_rate` 10. It involves no account of the test wallet. |
| `core-prepared-v2.psbt` | `core-prepared.psbt` as Core 32.1 writes it by default: a PSBT of version 2, the same payment field by field. |
| `core-prepared-v2-partial.psbt`, `core-prepared-v2-finalized.psbt`, `.hex` | `walletprocesspsbt` of the wallet `hw` on `core-prepared-v2.psbt`, with `finalize` false, then true, then `finalizepsbt`: Core's signature as a partial signature, the final script with the fields of version 2 kept, and the transaction alone. |
| `pending-payment-finalized-v2.psbt` | `pending-payment-finalized.psbt` as version 2, converted apart from hw: the fields of BIP 370 added to the maps, the unsigned transaction removed. Core 32.1 decodes it and extracts the same transaction. |
| `hw-payment-partial.psbt` | `walletprocesspsbt` of the wallet `hw` on the file hw writes for the payment of `Tx/PsbtFileSpec.hs` (pinned there in base64), with `finalize` false: Core's signature as a partial signature, as a hardware wallet leaves it. Core 32.1 wrote it, from the file with hw's field of the path of the input, which it keeps. |
| `hw-payment-finalized.psbt`, `.hex` | The same with `finalize` true, then `finalizepsbt`: the final script in place of the partial signature and the derivation records of the input, which Core clears, and the transaction alone. Core's finalizer leaves hw's fields: the input still carries its path. The transaction is the same as before hw wrote that field: the `.hex` file did not change. |
| `pending-payment-finalized.psbt`, `.hex` | The same for the payment of `Database/PendingTxSpec.hs` (8000 satoshis to the first address of another wallet, 11774 of change, at 1 satoshi per byte). It was made before hw wrote the path of an input in its field, and has none: it stands for a file that names no path of its signed input, which the wallet finds among its addresses. |
| `core-prepared-segwit.psbt` | `walletcreatefundedpsbt` of the wallet `hw84`, asked for version 0: 1 BTC from the coin of 50 BTC mined to /0/0 of the bip84 account, to a legacy address of `other`, with the change at /1/0 and `fee_rate` 10, which comes to 1440 satoshis for 144 virtual bytes. The spec signs this file with the bip84 account's key and pins the result, `coreSegwitPreparedSignedText` in `TestUtils.hs`: `analyzepsbt` finds that result complete, `finalizepsbt` extracts the transaction, and `testmempoolaccept` accepts it on that chain, 144 virtual bytes for 1440 satoshis. |
| `core-prepared-segwit-v2.psbt` | The same payment as Core 32.1 writes it by default, in version 2, the change first this time. |
| `core-prepared-segwit-v2-partial.psbt`, `core-prepared-segwit-v2-finalized.psbt`, `.hex` | `walletprocesspsbt` of the wallet `hw84` on the version 2 file, with `finalize` false, then true, then `finalizepsbt`: Core's signature of the P2WPKH coin as a partial signature, the final witness, and the transaction alone. |
| `hw-segwit-payment-partial.psbt` | `walletprocesspsbt` of the wallet `hw84` on the file hw writes for the payment of the bip84 account (`segwitPaymentText`), with `finalize` false: Core's signature as a partial signature, as a hardware wallet leaves it, with hw's field of the path of the input kept. |
| `hw-segwit-payment-finalized.psbt`, `.hex` | The same with `finalize` true, then `finalizepsbt`: the final witness in place of the partial signature and the derivation record of the input, which Core clears, and the transaction alone. The input still carries its path in hw's field. |

`test/Haskoin/Wallet/TestUtils.hs` pins the files that hw produces when it
signs `core-prepared.psbt`, `core-prepared-segwit.psbt` and
`core-prepared-v2.psbt` (`corePreparedSignedText`,
`coreSegwitPreparedSignedText`, `corePreparedSignedV2Text`). They were made
in three steps:

1. Each signed file was checked on that chain, as its row says. At that
   time, a signed input still carried its derivation record.
2. hw now writes a signed input as a finalizer leaves it, without that
   record. So a script cleared the record of the signed input in each checked
   file, independently of hw. The script was written from BIP 174, and
   rewrites the maps key by key. Bitcoin Core 32.1 decodes each file, finds
   it complete with `analyzepsbt`, and extracts with `finalizepsbt` the very
   transaction of the file checked on the chain.
3. hw also writes the path of the address it signed for, `/0/0`, in its field
   of subtype 2 of the signed input. Core's files do not have this field. The
   same kind of script added the field to the three pinned files. Core 32.1
   checked them again the same way: complete, and the same transactions.

`corePreparedSignedV2Text`, the file that hw produces when it signs
`core-prepared-v2.psbt`, was converted the same way from the version 0 file.
Core 32.1 checked it: `analyzepsbt` finds it complete, `finalizepsbt`
extracts the transaction of the version 0 file, and `testmempoolaccept`
accepts it.

The specs expect literal values for these files: the scripts, the derivation
records, the fees and the addresses. They are what `decodepsbt` shows for
the files, with the addresses of regtest rewritten for mainnet, as hw shows
them on Bitcoin.
