# Verification of the transaction signing test vectors

`verify_vectors.py` checks the JSON files in the parent folder independently
of hw, with the pure-Python Bitcoin primitives of
`../../message-signing/tools/btc.py`. Run it after adding or editing a vector,
from this folder (Python 3.8+, standard library only):

```sh
python3 verify_vectors.py ..
python3 verify_vectors.py ../../tx-sweep
```

The second command checks the sweep vectors of `test/data/tx-sweep`, which
have the same format.

For every vector, the script checks three things:

- The unsigned transaction is the signed one without its input scripts and
  witness.
- The `txid` is right.
- Every input of the signed transaction carries a valid, low-S signature by
  the key of the coin it spends (legacy sighash, Bitcoin Cash
  SIGHASH_FORKID, or BIP-143 for SegWit coins).

It also checks the `txInfo`: the type, the amount, the fee, and the size in
virtual bytes. For this, it derives the addresses of the wallet from the
`account`, with its type, and from the paths.

The `signed` transaction of a known-bug vector (`expect: error` with
`signed`) is verified as well. Error messages are hw's own behaviour, and
only `Tx/SigningSpec.hs` checks them.
