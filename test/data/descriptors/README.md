# Output descriptor test vectors

This folder holds two kinds of vector. The first is the descriptors of the
test wallet, which `test/Haskoin/Wallet/Keys/DescriptorSpec.hs` pins. The second
is the files that other wallets write for an account, which `importacc`
reads (`test/Haskoin/Wallet/WalletFileSpec.hs`).

## Checksums

The checksums that the specs expect were computed independently of hw, with
the Python code of the Checksum section of
[BIP 380](https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki)
(`descsum_create`). That code gives the BIP's own vector,
`raw(deadbeef)#89f8spxm`. Bitcoin Core 32.1 confirms them in three ways:

- `getdescriptorinfo` gives the same checksums.
- `deriveaddresses`, on the descriptor hw writes for the test account, gives
  the first addresses of `test/Haskoin/Wallet/TestUtils.hs`.
- `importdescriptors` takes that descriptor, in the `<0;1>` form of BIP 389,
  into a watch-only wallet.

## The files of Bitcoin Core

Core 32.1 wrote these files with `listdescriptors`, on a regtest node
started with `-connect=0 -listen=0 -dnsseed=0`:

| File | How it was made |
| --- | --- |
| `core-default-wallet.json` | `createwallet default`: the eight descriptors of a seed of Core's own, fingerprint `b079db60`, one per chain of its pkh, sh(wpkh), tr and wpkh accounts. hw takes the pkh ones as one account, and refuses the other kinds. |
| `core-watch-only.json` | `createwallet watch true true`, a wallet without private keys, then `importdescriptors` with the two descriptors of the test account on the test networks, `pkh([bc560f95/44h/1h/0h]tpubDCEeDp.../0/*)` and `/1/*`: the key of `testAccountXPubTest`. |

## The file of a Coldcard

`coldcard-export.json` has the shape of the generic JSON export of a
Coldcard (Advanced/Tools > Export Wallet > Generic JSON), which
`docs/generic-wallet-export.md` of the Coldcard firmware documents. It was
written by hand for the test wallet, since no Coldcard was at hand.

Its BIP49 and BIP84 sections hold the keys at m/49'/0'/0' and m/84'/0'/0',
their first addresses and their SLIP-132 forms (`_pub`). These were derived
independently of hw, with the Python tool of the message-signing vectors
(`../message-signing/tools/btc.py`). The checksums of the descriptors come
from the code of BIP 380. Core 32.1 confirms the checksums and the first
addresses.

The multisig sections of such a file, `bip48_1`, `bip48_2` and `bip45`, are
left out, since hw does not read them.
