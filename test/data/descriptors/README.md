# Output descriptor test vectors

The descriptors of the test wallet that `test/Haskoin/Wallet/DescriptorSpec.hs`
pins, and the files other wallets write for an account, which `importacc`
reads (`test/Haskoin/Wallet/LabelsSpec.hs`).

## Checksums

The checksums the specs expect were computed apart from hw with the Python
code of the Checksum section of
[BIP 380](https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki)
(`descsum_create`), which gives the BIP's own vector, `raw(deadbeef)#89f8spxm`.
Bitcoin Core 32.1 gives the same checksums with `getdescriptorinfo`, its
`deriveaddresses` on the descriptor hw writes for the test account gives the
first addresses of `test/Haskoin/Wallet/TestUtils.hs`, and its
`importdescriptors` takes that descriptor, in the `<0;1>` form of BIP 389,
into a watch-only wallet.

## The files of Bitcoin Core

Files Core 32.1 wrote with `listdescriptors` on a regtest node started with
`-connect=0 -listen=0 -dnsseed=0`:

| File | How it was made |
| --- | --- |
| `core-default-wallet.json` | `createwallet default`: the eight descriptors of a seed of Core's own, fingerprint `b079db60`, one per chain of its pkh, sh(wpkh), tr and wpkh accounts. hw takes the pkh ones as one account, and refuses the other kinds. |
| `core-watch-only.json` | `createwallet watch true true`, a wallet without private keys, then `importdescriptors` with the two descriptors of the test account on the test networks, `pkh([bc560f95/44h/1h/0h]tpubDCEeDp.../0/*)` and `/1/*`: the key of `testAccountXPubTest`. |

## The file of a Coldcard

`coldcard-export.json` is written by hand in the shape of the generic JSON
export of a Coldcard (Advanced/Tools > Export Wallet > Generic JSON), as
`docs/generic-wallet-export.md` of the Coldcard firmware documents it, for
the test wallet: no Coldcard was at hand. The keys of its BIP49 and BIP84
sections, m/49'/0'/0' and m/84'/0'/0', their first addresses and their
SLIP-132 forms (`_pub`) were derived apart from hw with the Python tool of
the message-signing vectors (`../message-signing/tools/btc.py`), and the
checksums of the descriptors with the code of BIP 380. Core 32.1 confirms
the checksums and the first addresses. The multisig sections of such a
file, `bip48_1`, `bip48_2` and `bip45`, are left out: hw does not read them.
