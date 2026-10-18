# Message signing test vectors

Test vectors for `signmsg` (Bitcoin Signed Message format, the legacy format of
BIP-322). They are loaded by `test/Haskoin/Wallet/MessageSigningSpec.hs`. Every
`.json` file in this directory is tested, so new vectors can be added without
changing the test code.

## Files

| File | Origin |
| --- | --- |
| `bitcoin-core.json` | Bitcoin Core unit and functional tests |
| `electrum.json` | Electrum test suite |
| `bitcoinjs-message.json` | bitcoinjs-message fixtures and README |
| `trezor.json` | Trezor firmware device tests |
| `python-bitcoinlib.json` | python-bitcoinlib `signmessage.json` |
| `hw-regression.json` | Produced by the `hw` binary itself (regression vectors) |
| `edge-cases.json` | Generated edge cases, plus addresses that must be rejected |

External vectors were extracted from the upstream sources at the commits given
in each `sourceUrl`. All vectors were independently verified with Bitcoin Core
25.0 `verifymessage` (regtest, JSON-RPC) and with a separate pure-Python
implementation of the format. The scripts that verify the vectors, and how to
add one, are in [tools/](tools/README.md).

## Format

Each file is a JSON object with a `description` and up to three lists:

- `vectors`: signing vectors.
- `hashVectors`: message hash only (`name`, `message`, `messageHash`, and
  `messageUtf8Hex` for non-ASCII messages).
- `rejected`: a key and an address that `signMessage` must refuse to sign for.

Fields of a signing vector:

| Field | Description |
| --- | --- |
| `name` | Unique name, prefixed by the origin |
| `network` | haskoin network name: `btc`, `btctest`, `bch`, `bchtest` |
| `key` | Private key, see below |
| `address` | Compressed P2PKH address of the key |
| `message` | Message text, signed as UTF-8 |
| `messageRepeat` | Optional. The message is `message` repeated this many times |
| `messageUtf8Hex` | Expected UTF-8 bytes of the full message. Required when the message is not plain ASCII (the test enforces it), so that an editor normalizing Unicode cannot silently change a vector |
| `messageHash` | Optional. Expected `SHA256d(VarString "Bitcoin Signed Message:\n" <> VarString message)` in hex |
| `signature` | 65-byte signature in base64 |
| `expect` | `match` (default), `verify` or `differ`, see below |
| `warnings` | Expected `signmsg` warnings: `whitespace`, `linebreak`. An absent field means no warnings, and the test checks it for every vector |
| `edgeCase` | Edge cases only. What the vector tests |
| `source`, `sourceUrl`, `comment` | Provenance and explanations |

A `key` is one of:

- `{"wif": ...}`: compressed WIF private key.
- `{"secretHex": ...}`: 32-byte secret in hex.
- `{"mnemonic", "passphrase", "path"}`: BIP-39 mnemonic and any BIP-32 path.
- `{"mnemonic", "passphrase", "account", "chain", "index"}`: derived the way
  `hw` does it: account key `m/44'/coin'/account'` from `signingKey`, then
  `chain/index`.

`expect` values:

- `match`: `hw` must produce exactly this signature (RFC6979 nonces, low S).
- `verify`: the signature was made with a different nonce (Electrum grinds for
  a low R value, python-bitcoinlib did not use RFC6979), so `hw` cannot
  reproduce it. It must be a valid legacy signature of `hw`'s message hash by
  the key.
- `differ`: the source signs different bytes than `hw` (Trezor normalizes
  messages to NFC). `hw` must produce a different signature.
