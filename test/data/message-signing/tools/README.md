# Verification of the message signing test vectors

These scripts verify the JSON files in the parent folder independently of hw.
Run them after adding or editing a vector.

## Requirements

- Python 3.8+ (standard library only)
- Bitcoin Core (`bitcoind` and `bitcoin-cli`), for
  `verify_with_bitcoin_core.py` only

## Files

| File | Purpose |
| --- | --- |
| `btc.py` | Independent pure-Python Bitcoin primitives used by the verifiers |
| `verify_vectors.py` | Independent verification of the vectors |
| `verify_with_bitcoin_core.py` | Verification with Bitcoin Core's `verifymessage` on a throwaway regtest node |

## Verify the vectors

Run from this folder:

```sh
python3 verify_vectors.py ..
python3 verify_with_bitcoin_core.py /path/to/bitcoin/bin ..
```

`verify_vectors.py` uses only `btc.py`. It checks the message bytes and hashes,
derives every key (from WIF, raw secrets or BIP-39 mnemonics) and its address,
and recovers the public key from every signature. Mnemonic keys are derived
with and without BIP-39 NFKD normalization, which detects the known deviation
documented in `edge/passphrase-unicode`. It does not check the expected
`warnings`, which are hw's own behaviour: `MessageSigningSpec.hs` does.

`verify_with_bitcoin_core.py` passes every vector to `verifymessage` over
JSON-RPC, so messages arrive byte for byte. Each address is converted to the
regtest address with the same public key hash. Vectors expected to `differ`
must be rejected; all others must be accepted.

## Add a vector

The vectors were originally produced by generator scripts (removed in October
2026 to avoid maintaining them). To add one:

1. Compute the expected values with hw's library, for example in `stack ghci`
   with `signedMessageHash` and `signMessage` from `Haskoin.Wallet.Signing`,
   or with the `hw signmsg` command on a temporary `HOME`.
2. Add it to the JSON file by hand, following the format in `../README.md`.
   A non-ASCII message needs its `messageUtf8Hex`, and every signing vector
   needs its `warnings`.
3. Run `stack test` and both verifiers above.
