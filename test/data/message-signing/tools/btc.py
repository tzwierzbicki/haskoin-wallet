"""Minimal pure-Python Bitcoin primitives for verifying the test vectors.

Written from the specifications, independently of hw and haskoin-core, so that
the verification does not share code (or bugs) with what it checks. Slow but
simple: only meant for tests.
"""

import hashlib
import hmac
import unicodedata

# --- secp256k1 ----------------------------------------------------------------

P = 2**256 - 2**32 - 977
N = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
G = (0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798,
     0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)


def point_add(a, b):
    """Add two points (None is the point at infinity)"""
    if a is None:
        return b
    if b is None:
        return a
    if a[0] == b[0] and (a[1] + b[1]) % P == 0:
        return None
    if a == b:
        slope = 3 * a[0] * a[0] * pow(2 * a[1], -1, P) % P
    else:
        slope = (b[1] - a[1]) * pow(b[0] - a[0], -1, P) % P
    x = (slope * slope - a[0] - b[0]) % P
    return x, (slope * (a[0] - x) - a[1]) % P


def point_mul(k, point):
    result = None
    while k:
        if k & 1:
            result = point_add(result, point)
        point = point_add(point, point)
        k >>= 1
    return result


def serialize_pubkey(point, compressed=True):
    x = point[0].to_bytes(32, "big")
    if compressed:
        return bytes([2 + (point[1] & 1)]) + x
    return b"\x04" + x + point[1].to_bytes(32, "big")


# --- Hashes -------------------------------------------------------------------

def sha256(data):
    return hashlib.sha256(data).digest()


def sha256d(data):
    return sha256(sha256(data))


def hash160(data):
    return hashlib.new("ripemd160", sha256(data)).digest()


# --- Bitcoin Signed Message ---------------------------------------------------

def compact_size(n):
    if n < 0xfd:
        return bytes([n])
    if n <= 0xffff:
        return b"\xfd" + n.to_bytes(2, "little")
    if n <= 0xffffffff:
        return b"\xfe" + n.to_bytes(4, "little")
    return b"\xff" + n.to_bytes(8, "little")


def message_hash(message: bytes) -> bytes:
    """SHA256d(CompactSize(24) "Bitcoin Signed Message:\\n" CompactSize(len) message)"""
    prefix = b"Bitcoin Signed Message:\n"
    return sha256d(compact_size(len(prefix)) + prefix + compact_size(len(message)) + message)


def recover_pubkey(signature: bytes, message: bytes):
    """Recover the public key from a 65-byte signature.
    Returns (point, compressed), or None if no public key can be recovered."""
    if len(signature) != 65 or not 27 <= signature[0] <= 34:
        return None
    flags = signature[0] - 27
    recid, compressed = flags & 3, bool(flags & 4)
    r = int.from_bytes(signature[1:33], "big")
    s = int.from_bytes(signature[33:], "big")
    if not (0 < r < N and 0 < s < N):
        return None
    x = r + (recid >> 1) * N
    if x >= P:
        return None
    y = pow((x ** 3 + 7) % P, (P + 1) // 4, P)
    if (y * y - x ** 3 - 7) % P != 0:
        return None
    if y % 2 != recid % 2:
        y = P - y
    e = int.from_bytes(message_hash(message), "big")
    # Q = r^-1 (s R - e G)
    q = point_mul(pow(r, -1, N), point_add(point_mul(s, (x, y)), point_mul((-e) % N, G)))
    return (q, compressed) if q else None


# --- Addresses ----------------------------------------------------------------

BASE58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
P2PKH_VERSIONS = {"btc": 0x00, "bch": 0x00, "btctest": 0x6f, "bchtest": 0x6f}
WIF_VERSIONS = {"btc": 0x80, "bch": 0x80, "btctest": 0xef, "bchtest": 0xef}


def base58check_encode(payload):
    data = payload + sha256d(payload)[:4]
    n = int.from_bytes(data, "big")
    text = ""
    while n:
        n, r = divmod(n, 58)
        text = BASE58[r] + text
    return "1" * (len(data) - len(data.lstrip(b"\0"))) + text


def base58check_decode(text):
    n = 0
    for c in text:
        n = n * 58 + BASE58.index(c)
    raw = n.to_bytes((n.bit_length() + 7) // 8, "big")
    raw = b"\0" * (len(text) - len(text.lstrip("1"))) + raw
    if sha256d(raw[:-4])[:4] != raw[-4:]:
        raise ValueError("Bad Base58Check checksum")
    return raw[:-4]


BECH32_CHARSET = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"


def _convert_bits(data, from_bits, to_bits, pad=True):
    acc = bits = 0
    out = []
    for value in data:
        acc = (acc << from_bits) | value
        bits += from_bits
        while bits >= to_bits:
            bits -= to_bits
            out.append((acc >> bits) & ((1 << to_bits) - 1))
    if pad and bits:
        out.append((acc << (to_bits - bits)) & ((1 << to_bits) - 1))
    return out


def cashaddr_decode(address):
    """Decode a CashAddr address (prefix:payload). Returns (type, hash)."""
    prefix, payload = address.split(":")
    values = [BECH32_CHARSET.index(c) for c in payload]
    checksum = 1
    for value in [ord(c) & 31 for c in prefix] + [0] + values:
        top = checksum >> 35
        checksum = ((checksum & 0x07ffffffff) << 5) ^ value
        for i, g in enumerate([0x98f2bc8e61, 0x79b76d99e2, 0xf33e5fb3c4, 0xae2eabe2a8, 0x1e4f43e470]):
            if top & (1 << i):
                checksum ^= g
    if checksum ^ 1 != 0:
        raise ValueError("Bad CashAddr checksum")
    raw = bytes(_convert_bits(values[:-8], 5, 8, pad=False))
    return raw[0] >> 3, raw[1:]


def bech32_encode(hrp, witness_version, program):
    """Encode a SegWit v0 address (BIP-173)"""
    generator = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]

    def polymod(values):
        checksum = 1
        for value in values:
            top = checksum >> 25
            checksum = (checksum & 0x1ffffff) << 5 ^ value
            for i in range(5):
                checksum ^= generator[i] if (top >> i) & 1 else 0
        return checksum

    data = [witness_version] + _convert_bits(program, 8, 5)
    expanded = [ord(x) >> 5 for x in hrp] + [0] + [ord(x) & 31 for x in hrp]
    mod = polymod(expanded + data + [0] * 6) ^ 1
    checksum = [(mod >> 5 * (5 - i)) & 31 for i in range(6)]
    return hrp + "1" + "".join(BECH32_CHARSET[d] for d in data + checksum)


def p2pkh_hash(address):
    """The public key hash of a P2PKH address (Base58 or CashAddr)"""
    if ":" in address:
        kind, h = cashaddr_decode(address)
        if kind != 0:
            raise ValueError("Not a P2PKH CashAddr address")
        return h
    raw = base58check_decode(address)
    if raw[0] not in (0x00, 0x6f) or len(raw) != 21:
        raise ValueError("Not a P2PKH address")
    return raw[1:]


# --- Keys ---------------------------------------------------------------------

COIN_TYPES = {"btc": 0, "btctest": 1, "bch": 145, "bchtest": 1}
HARDENED = 2**31


def bip32_derive(seed, path):
    """Derive a private key from a BIP-32 seed and a list of indices"""
    digest = hmac.new(b"Bitcoin seed", seed, hashlib.sha512).digest()
    key, chain_code = int.from_bytes(digest[:32], "big"), digest[32:]
    for i in path:
        if i >= HARDENED:
            data = b"\0" + key.to_bytes(32, "big")
        else:
            data = serialize_pubkey(point_mul(key, G))
        digest = hmac.new(chain_code, data + i.to_bytes(4, "big"), hashlib.sha512).digest()
        key, chain_code = (int.from_bytes(digest[:32], "big") + key) % N, digest[32:]
    return key


def bip39_seed(mnemonic, passphrase, nfkd=True):
    """BIP-39 seed. BIP-39 requires NFKD normalization; nfkd=False computes the
    seed without it (to detect implementations that do not normalize)."""
    def normalize(text):
        return unicodedata.normalize("NFKD", text) if nfkd else text
    return hashlib.pbkdf2_hmac("sha512", normalize(mnemonic).encode(),
                               ("mnemonic" + normalize(passphrase)).encode(), 2048)


def parse_path(path):
    """Parse a path like m/44'/0'/0'/0/0 into a list of indices"""
    return [int(p[:-1]) + HARDENED if p.endswith("'") else int(p) for p in path.split("/")[1:]]


def private_keys(key, network):
    """The candidate private keys of a key specification (see ../README.md).
    Mnemonic keys give two candidates: with and without BIP-39 NFKD
    normalization (they are equal for ASCII mnemonics and passphrases)."""
    if "secretHex" in key:
        return [int(key["secretHex"], 16)]
    if "wif" in key:
        raw = base58check_decode(key["wif"])
        if raw[0] != WIF_VERSIONS[network] or len(raw) != 34 or raw[-1] != 1:
            raise ValueError("Not a compressed WIF key for this network")
        return [int.from_bytes(raw[1:33], "big")]
    if "path" in key:
        path = parse_path(key["path"])
    else:
        path = [44 + HARDENED, COIN_TYPES[network] + HARDENED, key["account"] + HARDENED, key["chain"], key["index"]]
    return [bip32_derive(bip39_seed(key["mnemonic"], key.get("passphrase", ""), nfkd), path)
            for nfkd in (True, False)]
