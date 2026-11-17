# BIP 329 test vectors

`bip-0329.jsonl` is the wallet label export of the Test Vectors section of
[BIP 329](https://github.com/bitcoin/bips/blob/master/bip-0329.mediawiki),
copied as published there (BSD-2-Clause), with its spacing. It has one
record of each type the BIP defines. It also has two transactions of two
accounts of one seed, which their origins tell apart.

`test/Haskoin/Wallet/Bip329Spec.hs` reads the file with the parser of
`Haskoin.Wallet.Bip329`, and checks each record against the fields the BIP
lists.

The wallet of the vectors is not the test wallet. So the import of a file
into a wallet is tested with other records, written for the test seed, in
`test/Haskoin/Wallet/LabelsSpec.hs` and the workflow specs.
