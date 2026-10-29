#!/usr/bin/env python3
"""Independently verify the message signing test vectors.

Usage: verify_vectors.py VECTOR_DIR

Uses only btc.py (pure Python, no hw or haskoin-core code). For every vector it
checks the message bytes and hash, derives the key's address, and recovers the
public key from the signature. For every verification vector it gives the
verdict the way hw must: valid, invalid, or refused. Exits with an error if
anything is wrong.
"""

import base64
import binascii
import json
import os
import sys

import btc


def main():
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    directory = sys.argv[1]
    all_problems = []
    for filename in sorted(f for f in os.listdir(directory) if f.endswith(".json")):
        with open(os.path.join(directory, filename), encoding="utf-8") as f:
            doc = json.load(f)
        problems = []
        for kind, check in (("hashVectors", hash_vector_problems), ("vectors", vector_problems),
                            ("rejected", rejected_problems), ("verification", verification_problems)):
            for v in doc.get(kind, []):
                problems += named(v, safely(check, v))
        count = sum(len(doc.get(k, [])) for k in ("hashVectors", "vectors", "rejected", "verification"))
        status = "ok" if not problems else f"{len(problems)} problem{'s' if len(problems) > 1 else ''}"
        print(f"{filename}: {count} vectors, {status}")
        all_problems += [f"{filename}: {p}" for p in problems]
    for p in all_problems:
        print("  " + p)
    sys.exit(1 if all_problems else 0)


def named(v, problems):
    return [f"{v['name']}: {p}" for p in problems]


def safely(check, v):
    """Run a check; a vector that can not be read (e.g. an invalid key) is a
    problem, not a crash"""
    try:
        return check(v)
    except (ValueError, KeyError, TypeError) as e:
        return [f"could not be checked: {type(e).__name__}: {e}"]


def message_bytes(v):
    return (v["message"] * v.get("messageRepeat", 1)).encode()


def message_problems(v):
    problems = []
    data = message_bytes(v)
    if "messageUtf8Hex" in v and data.hex() != v["messageUtf8Hex"]:
        problems.append("UTF-8 bytes differ from messageUtf8Hex")
    if "messageUtf8Hex" not in v and not data.isascii():
        problems.append("non-ASCII message without messageUtf8Hex")
    if "messageHash" in v and btc.message_hash(data).hex() != v["messageHash"]:
        problems.append("wrong messageHash")
    return problems


def hash_vector_problems(v):
    return message_problems(v)


def vector_problems(v):
    problems = message_problems(v)
    address_hash = btc.p2pkh_hash(v["address"])
    keys = btc.private_keys(v["key"], v["network"])
    matching = [k for k in keys if btc.hash160(btc.serialize_pubkey(btc.point_mul(k, btc.G))) == address_hash]
    if not matching:
        problems.append("the address does not belong to the key")
    elif matching[0] != keys[0] and "KNOWN BIP-39 DEVIATION" not in v.get("comment", ""):
        problems.append("the address only matches without BIP-39 NFKD normalization")
    signature = base64.b64decode(v["signature"])
    recovered = btc.recover_pubkey(signature, message_bytes(v))
    valid = (recovered is not None and recovered[1]
             and btc.hash160(btc.serialize_pubkey(recovered[0])) == address_hash)
    expect = v.get("expect", "match")
    if expect == "differ":
        if valid:
            problems.append("expected to differ, but the signature is valid for this message")
    elif not valid:
        problems.append("the signature is not valid")
    elif expect == "match":
        if signature[0] not in (31, 32):
            problems.append("hw only produces header bytes 31 and 32")
        if int.from_bytes(signature[33:], "big") > btc.N // 2:
            problems.append("hw only produces low-S signatures")
    return problems


def rejected_problems(v):
    """The address must not be the compressed P2PKH address of the key, and must
    be what the vector says it is"""
    key = btc.private_keys(v["key"], v["network"])[0]
    pub = btc.point_mul(key, btc.G)
    compressed_hash = btc.hash160(btc.serialize_pubkey(pub))
    address = v["address"]
    if address.startswith("bc1"):
        kind = "P2WPKH of the key" if address == btc.bech32_encode("bc", 0, compressed_hash) else "other SegWit"
    else:
        raw = btc.base58check_decode(address)
        if raw[0] == 0x05:
            redeem = b"\x00\x14" + compressed_hash
            kind = "P2SH-P2WPKH of the key" if raw[1:] == btc.hash160(redeem) else "other P2SH"
        elif raw[1:] == compressed_hash:
            return ["the address is the compressed P2PKH address of the key: it must be accepted"]
        elif raw[1:] == btc.hash160(btc.serialize_pubkey(pub, compressed=False)):
            kind = "uncompressed P2PKH of the key"
        else:
            kind = "P2PKH of another key"
    expected = {"uncompressed-p2pkh": "uncompressed P2PKH of the key", "p2wpkh": "P2WPKH of the key",
                "p2sh-p2wpkh": "P2SH-P2WPKH of the key", "other-key": "P2PKH of another key",
                "p2sh-bitcoin-core": "other P2SH", "d-1-p2wpkh": "P2WPKH of the key"}
    wanted = expected.get(v["name"].split("/")[-1])
    if wanted and kind != wanted:
        return [f"the address is a {kind} address, not {wanted}"]
    return []


def verification_problems(v):
    """The verdict of a verification vector: refused when the address is not
    P2PKH or the signature is not 65 bytes in base64, else valid when the
    signature recovers the key of the address, compressed or not as its header
    byte says, else invalid"""
    problems = message_problems(v)
    try:
        address_hash = btc.p2pkh_hash(v["address"])
        signature = base64.b64decode(v["signature"], validate=True)
        if len(signature) != 65:
            raise ValueError(f"the signature has {len(signature)} bytes")
    except (ValueError, binascii.Error):
        verdict = "refused"
    else:
        recovered = btc.recover_pubkey(signature, message_bytes(v))
        valid = recovered is not None and btc.hash160(btc.serialize_pubkey(*recovered)) == address_hash
        verdict = "valid" if valid else "invalid"
    if verdict != v["result"]:
        problems.append(f"expected {v['result']}, got {verdict}")
    return problems


if __name__ == "__main__":
    main()
