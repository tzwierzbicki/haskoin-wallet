# PSBT test vectors

The PSBT format itself, `Haskoin.Transaction.Psbt`, lives in haskoin-core,
with the test vectors of [BIP 174](https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki)
(`data/psbt_bip174.json` there) and the spec that checks them. The files
here are those of hw's own transaction files.

## The file of hw

`test/Haskoin/Wallet/Tx/PsbtFileSpec.hs` pins the PSBT hw writes for the
payment of the test wallet, in base64. It was produced by hw and checked with
Bitcoin Core's `decodepsbt` (the unsigned transaction, the previous
transaction of the input, the derivation records with the fingerprint
`bc560f95` and the paths `m/44'/0'/0'/0/0` and `m/44'/0'/0'/1/0`, the sighash
type, and hw's proprietary fields under the identifier `6877`, "hw"). To
check it again, run a regtest node and decode the string:

```sh
bitcoind -regtest -datadir=/tmp/hw-regtest -daemon -connect=0 -listen=0
bitcoin-cli -regtest -datadir=/tmp/hw-regtest decodepsbt BASE64
```

The public keys of the test wallet the specs name were derived apart from hw
with the Python tool of the message-signing vectors
(`../message-signing/tools/btc.py`).

Both pinned files carry the path of the address their input spends from,
`/0/0`, in hw's field of subtype 2 of the input: the key `fc02687702` and
the value `0000000000000000`, the chain and the index, 4 bytes each,
little-endian. That field was added to the files checked before, apart from
hw, by a script written from BIP 174 that rewrites the map of the input key
by key. `decodepsbt` of Bitcoin Core 32.1 shows the field among the
proprietary fields of the input and the rest of each file unchanged, and
`analyzepsbt` names the signer as the next role.

`segwitPaymentText` in `test/Haskoin/Wallet/Tx/PsbtFileSpec.hs` pins the
file hw writes for the same payment by the bip84 account of the test
wallet, checked the same way with `decodepsbt`: the coin of its input
both as its whole transaction and as itself (`witness_utxo`), the
derivation records `m/84'/0'/0'/0/0` and `m/84'/0'/0'/1/0`, and hw's
fields. Its public keys were derived apart from hw the same way.

## The files of Bitcoin Core, in `core/`

Files Bitcoin Core wrote on a regtest chain, for the specs that read and
sign the files of other wallets (`Tx/PsbtFileSpec.hs`,
`Database/PendingTxSpec.hs`, `Workflow/SigningSpec.hs`). Each is the base64
text Core's RPCs give, as a user would save it. Core 25.0 wrote them. Core
32.1 writes the same bytes, signs and finalizes the same, and accepts the
same transaction, when `walletcreatefundedpsbt` is asked for a PSBT of
version 0 with its `psbt_version` argument. Its default is version 2 (BIP
370) since Core 32.0, which hw reads too: the files named `-v2` are those.

The wallet `hw` of that node holds the keys of the test account through the
descriptors `pkh([bc560f95/44h/0h/0h]tprv.../0/*)` and `.../1/*`, the
account's key `xprv9yHxeaL...` in the encoding of regtest, so that the keys
and the derivation records are those of the test wallet. The wallet `hw84`
of the `-segwit` files holds those of its bip84 account the same way,
`wpkh([bc560f95/84h/0h/0h]tprv.../0/*)` and `.../1/*`, the key
`xprv9yQtifF...` in the encoding of regtest
(`tprv8g5qVzZb7thKDKxbZnD4BBfZjesetceTNJkuNS6CwvsDzwTYAd8awzifSNCxyNuZngEcn5BqS9MhuD6Eiz9oqLVDDz3bvvSWDzNkB4MHYLB`);
it mined 101 blocks to its address /0/0 on a chain of its own, so that the
coin of block 1 is its one mature coin, and `other` holds a legacy address
there. Core 32.1 wrote these files. A second wallet,
`other`, has keys of its own, the descriptors
`pkh(tprv8ZgxMBicQKsPdLA1DWtYbSdVVfjzd7dNqEvvBFL6V6YjwLkiQnzT6y9NgSHFG27K3sKpofaeeZjrSZkomqA7YRtncLyEEnuqhRWUeMMHq3T/44'/1'/0'/0/*)`
and `.../1/*`, the fingerprint `b7253d71`. `other` mined blocks 1 to 101 to
its first address, then `hw` mined 101 blocks to its address /0/0, so that
the coin of block 102 is the one mature coin of `hw`. The change of each
file is at the first change address of its wallet, which a wallet that made
a file before gives again with the `changeAddress` option.

| File | How it was made |
| --- | --- |
| `core-prepared.psbt` | `walletcreatefundedpsbt` of the wallet `hw`: 1 BTC from the coin of 50 BTC mined to /0/0 of the test account, to a legacy address of `other`, with the change at /1/0 and `fee_rate` 10. The spec signs it with the test account's key into this file, and pins the result: `analyzepsbt` finds that result complete, `finalizepsbt` extracts the transaction, and `testmempoolaccept` accepts it on that chain. |
| `core-outside.psbt` | `walletcreatefundedpsbt` of the wallet `other`: 2.5 BTC of its own coin to another of its legacy addresses, `fee_rate` 10. It involves no account of the test wallet. |
| `core-prepared-v2.psbt` | `core-prepared.psbt` as Core 32.1 writes it by default: a PSBT of version 2, the same payment field by field. |
| `core-prepared-v2-partial.psbt`, `core-prepared-v2-finalized.psbt`, `.hex` | `walletprocesspsbt` of the wallet `hw` on `core-prepared-v2.psbt`, with `finalize` false, then true, then `finalizepsbt`: Core's signature as a partial signature, the final script with the fields of version 2 kept, and the transaction alone. |
| `pending-payment-finalized-v2.psbt` | `pending-payment-finalized.psbt` as version 2, converted apart from hw: the fields of BIP 370 added to the maps, the unsigned transaction removed. Core 32.1 decodes it and extracts the same transaction. |

The files hw signs into `core-prepared.psbt`, `core-prepared-segwit.psbt`
and `core-prepared-v2.psbt` are pinned in `test/Haskoin/Wallet/TestUtils.hs`
(`corePreparedSignedText`, `coreSegwitPreparedSignedText`,
`corePreparedSignedV2Text`). Each was checked on that chain as its row says,
when a signed input still carried its derivation record. hw now writes a
signed input as a finalizer leaves it, without that record. The pinned files
are the checked ones with the record of the signed input cleared apart from
hw, by a script written from BIP 174 that rewrites the maps key by key.
Bitcoin Core 32.1 decodes each, finds it complete with `analyzepsbt`, and
extracts with `finalizepsbt` the very transaction of the file checked on the
chain. hw also writes the path of the address it signed for, `/0/0`, in its
field of subtype 2 of the signed input, which Core's files do not have. The
same kind of script added that field to the three pinned files, and Core
32.1 checked them again the same way: complete, and the same transactions.

`corePreparedSignedV2Text` in `test/Haskoin/Wallet/TestUtils.hs`, the file
hw signs into `core-prepared-v2.psbt`, was converted the same way from the
version 0 one, and checked with Core 32.1: `analyzepsbt` finds it complete,
`finalizepsbt` extracts the transaction of the version 0 one, and
`testmempoolaccept` accepts it.
| `hw-payment-partial.psbt` | `walletprocesspsbt` of the wallet `hw` on the file hw writes for the payment of `Tx/PsbtFileSpec.hs` (pinned there in base64), with `finalize` false: Core's signature as a partial signature, as a hardware wallet leaves it. Core 32.1 wrote it, from the file with hw's field of the path of the input, which it keeps. |
| `hw-payment-finalized.psbt`, `.hex` | The same with `finalize` true, then `finalizepsbt`: the final script in place of the partial signature and the derivation records of the input, which Core clears, and the transaction alone. Core's finalizer leaves hw's fields: the input still carries its path. The transaction is the same as before hw wrote that field: the `.hex` file did not change. |
| `pending-payment-finalized.psbt`, `.hex` | The same for the payment of `Database/PendingTxSpec.hs` (8000 satoshis to the first address of another wallet, 11774 of change, at 1 satoshi per byte). It was made before hw wrote the path of an input in its field, and has none: it stands for a file that names no path of its signed input, which the wallet finds among its addresses. |
| `core-prepared-segwit.psbt` | `walletcreatefundedpsbt` of the wallet `hw84`, asked for version 0: 1 BTC from the coin of 50 BTC mined to /0/0 of the bip84 account, to a legacy address of `other`, with the change at /1/0 and `fee_rate` 10, which comes to 1440 satoshis for 144 virtual bytes. The spec signs it with the bip84 account's key into this file and pins the result, `coreSegwitPreparedSignedText` in `TestUtils.hs`: `analyzepsbt` finds that result complete, `finalizepsbt` extracts the transaction, and `testmempoolaccept` accepts it on that chain, 144 virtual bytes for 1440 satoshis. |
| `core-prepared-segwit-v2.psbt` | The same payment as Core 32.1 writes it by default, in version 2, the change first this time. |
| `core-prepared-segwit-v2-partial.psbt`, `core-prepared-segwit-v2-finalized.psbt`, `.hex` | `walletprocesspsbt` of the wallet `hw84` on the version 2 file, with `finalize` false, then true, then `finalizepsbt`: Core's signature of the P2WPKH coin as a partial signature, the final witness, and the transaction alone. |
| `hw-segwit-payment-partial.psbt` | `walletprocesspsbt` of the wallet `hw84` on the file hw writes for the payment of the bip84 account (`segwitPaymentText`), with `finalize` false: Core's signature as a partial signature, as a hardware wallet leaves it, with hw's field of the path of the input kept. |
| `hw-segwit-payment-finalized.psbt`, `.hex` | The same with `finalize` true, then `finalizepsbt`: the final witness in place of the partial signature and the derivation record of the input, which Core clears, and the transaction alone. The input still carries its path in hw's field. |

The literal values the specs expect for these files, the scripts, the
derivation records, the fees and the addresses, are what `decodepsbt` shows
for them, with the addresses of regtest rewritten for mainnet, as hw shows
them on Bitcoin.
