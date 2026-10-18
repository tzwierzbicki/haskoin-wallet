# Verification of the transaction signing test vectors

`verify_vectors.py` checks the JSON files in the parent folder independently
of hw, with the pure-Python Bitcoin primitives of
`../../message-signing/tools/btc.py`. Run it after adding or editing a vector,
from this folder (Python 3.8+, standard library only):

```sh
python3 verify_vectors.py ..
```

For every vector it checks that the unsigned transaction is the signed one
without its input scripts and witness, that the `txid` is right, that every
previous transaction is referenced, and that every input of the signed
transaction carries a valid, low-S signature by the key of the coin it spends
(legacy sighash, Bitcoin Cash SIGHASH_FORKID, or BIP-143 for SegWit coins). It
derives the wallet's addresses from the `account` and the paths to check the
`txInfo` type, amount and fee. The `signed` transaction of a known-bug vector
(`expect: error` with `signed`) is verified as well. Error messages are hw's
own behaviour and are only checked by `TxSigningSpec.hs`.
