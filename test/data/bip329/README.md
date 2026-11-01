# BIP 329 test vectors

`bip-0329.jsonl` is the wallet label export of the Test Vectors section of
[BIP 329](https://github.com/bitcoin/bips/blob/master/bip-0329.mediawiki),
as published there (BSD-2-Clause), with its spacing. It has one record of
each type the BIP defines, and two transactions of two accounts of one
seed, told apart by their origins.

`test/Haskoin/Wallet/Bip329Spec.hs` reads it with the parser of
`Haskoin.Wallet.Bip329`, and checks each record against the fields the BIP
lists. The wallet of the vectors is not the test wallet, so the import of
the file into a wallet is tested with records written for the test seed
instead, in `test/Haskoin/Wallet/LabelsSpec.hs` and the workflow specs.
