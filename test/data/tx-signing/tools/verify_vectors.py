#!/usr/bin/env python3
"""Independently verify the transaction signing test vectors.

Usage: verify_vectors.py VECTOR_DIR

Uses only the pure-Python primitives of ../../message-signing/tools/btc.py (no
hw or haskoin-core code). For every vector with a signed transaction it checks
the unsigned transaction, the txid, every input signature and the txInfo
summary. Exits with an error if anything is wrong.
"""

import json
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "message-signing", "tools"))
import btc  # noqa: E402

FORKID_NETWORKS = ("bch", "bchtest")


def main():
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    directory = sys.argv[1]
    all_problems = []
    for filename in sorted(f for f in os.listdir(directory) if f.endswith(".json")):
        with open(os.path.join(directory, filename), encoding="utf-8") as f:
            vectors = json.load(f)["vectors"]
        problems = []
        for v in vectors:
            problems += [f"{v['name']}: {p}" for p in safely(vector_problems, v)]
        status = "ok" if not problems else f"{len(problems)} problem{'s' if len(problems) > 1 else ''}"
        print(f"{filename}: {len(vectors)} vectors, {status}")
        all_problems += [f"{filename}: {p}" for p in problems]
    for p in all_problems:
        print("  " + p)
    sys.exit(1 if all_problems else 0)


def safely(check, v):
    try:
        return check(v)
    except (ValueError, KeyError, TypeError, IndexError, AssertionError) as e:
        return [f"could not be checked: {type(e).__name__}: {e}"]


# --- Transactions -------------------------------------------------------------

def read_varint(b, i):
    n = b[i]
    if n < 0xFD:
        return n, i + 1
    size = {0xFD: 2, 0xFE: 4, 0xFF: 8}[n]
    return int.from_bytes(b[i + 1:i + 1 + size], "little"), i + 1 + size


def read_bytes(b, i):
    n, i = read_varint(b, i)
    return b[i:i + n], i + n


def parse_tx(b):
    """A transaction as a dict: version, ins (prev, idx, script, seq, witness), outs (value, script), locktime"""
    version, i = int.from_bytes(b[:4], "little"), 4
    segwit = b[4] == 0 and b[5] == 1
    if segwit:
        i = 6
    n, i = read_varint(b, i)
    ins = []
    for _ in range(n):
        prev, idx = b[i:i + 32], int.from_bytes(b[i + 32:i + 36], "little")
        script, i = read_bytes(b, i + 36)
        seq, i = int.from_bytes(b[i:i + 4], "little"), i + 4
        ins.append(dict(prev=prev, idx=idx, script=script, seq=seq, witness=[]))
    n, i = read_varint(b, i)
    outs = []
    for _ in range(n):
        value, i = int.from_bytes(b[i:i + 8], "little"), i + 8
        script, i = read_bytes(b, i)
        outs.append(dict(value=value, script=script))
    if segwit:
        for inp in ins:
            n, i = read_varint(b, i)
            for _ in range(n):
                item, i = read_bytes(b, i)
                inp["witness"].append(item)
    locktime = int.from_bytes(b[i:i + 4], "little")
    assert i + 4 == len(b), "trailing bytes after the transaction"
    return dict(version=version, ins=ins, outs=outs, locktime=locktime)


def serialize(tx, scripts=None):
    """Serialize without witness data. scripts: input index -> scriptSig to use (others empty); None keeps them"""
    b = tx["version"].to_bytes(4, "little") + btc.compact_size(len(tx["ins"]))
    for i, inp in enumerate(tx["ins"]):
        script = inp["script"] if scripts is None else scripts.get(i, b"")
        b += inp["prev"] + inp["idx"].to_bytes(4, "little") + btc.compact_size(len(script)) + script
        b += inp["seq"].to_bytes(4, "little")
    b += btc.compact_size(len(tx["outs"]))
    for out in tx["outs"]:
        b += out["value"].to_bytes(8, "little") + btc.compact_size(len(out["script"])) + out["script"]
    return b + tx["locktime"].to_bytes(4, "little")


def txid(tx):
    return btc.sha256d(serialize(tx))[::-1].hex()


def pushes(script):
    """The data pushes of a script made only of pushes (a scriptSig)"""
    items, i = [], 0
    while i < len(script):
        n = script[i]
        if 1 <= n <= 75:
            items.append(script[i + 1:i + 1 + n])
            i += 1 + n
        elif n == 0x4C:
            n = script[i + 1]
            items.append(script[i + 2:i + 2 + n])
            i += 2 + n
        else:
            raise ValueError(f"unexpected opcode {n:#x} in scriptSig")
    return items


def p2pkh_script(hash160):
    return b"\x76\xa9\x14" + hash160 + b"\x88\xac"


# --- Signature hashes ---------------------------------------------------------

def legacy_sighash(tx, i, script_code, hash_type):
    return btc.sha256d(serialize(tx, {i: script_code}) + hash_type.to_bytes(4, "little"))


def bip143_sighash(tx, i, script_code, value, hash_type):
    """BIP-143 digest, also used by Bitcoin Cash with SIGHASH_FORKID"""
    inp = tx["ins"][i]
    prevouts = b"".join(x["prev"] + x["idx"].to_bytes(4, "little") for x in tx["ins"])
    sequences = b"".join(x["seq"].to_bytes(4, "little") for x in tx["ins"])
    outputs = b"".join(o["value"].to_bytes(8, "little") + btc.compact_size(len(o["script"])) + o["script"]
                       for o in tx["outs"])
    preimage = (tx["version"].to_bytes(4, "little") + btc.sha256d(prevouts) + btc.sha256d(sequences)
                + inp["prev"] + inp["idx"].to_bytes(4, "little")
                + btc.compact_size(len(script_code)) + script_code + value.to_bytes(8, "little")
                + inp["seq"].to_bytes(4, "little") + btc.sha256d(outputs)
                + tx["locktime"].to_bytes(4, "little") + hash_type.to_bytes(4, "little"))
    return btc.sha256d(preimage)


# --- ECDSA --------------------------------------------------------------------

def parse_pubkey(data):
    if len(data) == 65 and data[0] == 4:
        return int.from_bytes(data[1:33], "big"), int.from_bytes(data[33:], "big")
    if len(data) == 33 and data[0] in (2, 3):
        x = int.from_bytes(data[1:], "big")
        y = pow((x ** 3 + 7) % btc.P, (btc.P + 1) // 4, btc.P)
        if y % 2 != data[0] % 2:
            y = btc.P - y
        return x, y
    raise ValueError("invalid public key encoding")


def parse_der(sig):
    """(r, s) of a strict DER signature"""
    assert sig[0] == 0x30 and sig[1] == len(sig) - 2 and sig[2] == 0x02, "not a DER signature"
    r_len = sig[3]
    r = int.from_bytes(sig[4:4 + r_len], "big")
    assert sig[4 + r_len] == 0x02, "not a DER signature"
    s_len = sig[5 + r_len]
    assert 6 + r_len + s_len == len(sig), "not a DER signature"
    s = int.from_bytes(sig[6 + r_len:], "big")
    return r, s


def ecdsa_verify(pub, digest, r, s):
    e = int.from_bytes(digest, "big")
    w = pow(s, -1, btc.N)
    point = btc.point_add(btc.point_mul(e * w % btc.N, btc.G), btc.point_mul(r * w % btc.N, pub))
    return point is not None and point[0] % btc.N == r


# --- Checks -------------------------------------------------------------------

def input_problems(tx, i, prevout, network):
    """Check the signature of input i spending prevout. Returns (problems, hash160 of the signing key)"""
    inp, spk = tx["ins"][i], prevout["script"]
    forkid = network in FORKID_NETWORKS
    hash_type = 0x41 if forkid else 0x01
    if len(spk) == 25 and spk[:3] == b"\x76\xa9\x14" and spk[23:] == b"\x88\xac":  # P2PKH
        sig, pub = pushes(inp["script"])
        key_hash, script_code, segwit = spk[3:23], spk, False
    elif spk[-1] == 0xAC and len(spk) == spk[0] + 2:  # P2PK
        (sig,), pub = pushes(inp["script"]), spk[1:-1]
        key_hash, script_code, segwit = btc.hash160(pub), spk, False
    elif len(spk) == 22 and spk[:2] == b"\x00\x14":  # P2WPKH
        assert not inp["script"], "a P2WPKH input must have an empty scriptSig"
        sig, pub = inp["witness"]
        key_hash, script_code, segwit = spk[2:], p2pkh_script(spk[2:]), True
    elif len(spk) == 23 and spk[:2] == b"\xa9\x14" and spk[22] == 0x87:  # P2SH, expected P2SH-P2WPKH
        (redeem,) = pushes(inp["script"])
        assert btc.hash160(redeem) == spk[2:22], "the redeem script does not match the P2SH output"
        assert len(redeem) == 22 and redeem[:2] == b"\x00\x14", "only P2SH-P2WPKH inputs are supported"
        sig, pub = inp["witness"]
        key_hash, script_code, segwit = redeem[2:], p2pkh_script(redeem[2:]), True
    else:
        return [f"input {i}: unsupported previous output script {spk.hex()}"], None
    problems = []
    if btc.hash160(pub) != key_hash:
        problems.append(f"input {i}: the public key does not match the coin")
    if sig[-1] != hash_type:
        problems.append(f"input {i}: sighash type {sig[-1]:#x}, expected {hash_type:#x}")
    r, s = parse_der(sig[:-1])
    if s > btc.N // 2:
        problems.append(f"input {i}: the signature is not low-S")
    if segwit or forkid:
        digest = bip143_sighash(tx, i, script_code, prevout["value"], hash_type)
    else:
        digest = legacy_sighash(tx, i, script_code, hash_type)
    if not ecdsa_verify(parse_pubkey(pub), digest, r, s):
        problems.append(f"input {i}: invalid signature")
    return problems, key_hash


def path_key_hashes(v):
    """hash160 of the wallet's own addresses, from the account and the paths"""
    account = v["account"]

    def key_hash(path):
        _, chain, index = path.split("/")
        key = dict(account, chain=int(chain), index=int(index))
        private = btc.private_keys(key, v["network"])[0]
        return btc.hash160(btc.serialize_pubkey(btc.point_mul(private, btc.G)))

    tsd = v["txSignData"]
    return {key_hash(p) for p in tsd["inputpaths"]}, {key_hash(p) for p in tsd["outputpaths"]}


def vector_problems(v):
    if "signed" not in v:
        return []  # hw's error messages are checked by TxSigningSpec.hs only
    tsd, expected = v["txSignData"], v["signed"]
    tx = parse_tx(bytes.fromhex(expected["tx"]))
    problems = []
    # An input of another party may already be signed in the unsigned transaction
    if serialize(tx, {}) != serialize(parse_tx(bytes.fromhex(tsd["tx"])), {}):
        problems.append("the unsigned transaction is not the signed one without input scripts")
    if txid(tx) != expected["txid"]:
        problems.append("wrong txid")
    previous = {}
    for raw in tsd["txinputs"]:
        prev = parse_tx(bytes.fromhex(raw))
        previous[txid(prev)] = prev
    own_inputs, own_outputs = path_key_hashes(v)
    in_sum = my_in = 0
    spent_hashes = set()
    for i, inp in enumerate(tx["ins"]):
        prev = previous.get(inp["prev"][::-1].hex())
        if prev is None:
            problems.append(f"input {i}: previous transaction missing")
            continue
        prevout = prev["outs"][inp["idx"]]
        in_sum += prevout["value"]
        input_problems_, key_hash = input_problems(tx, i, prevout, v["network"])
        problems += input_problems_
        spent_hashes.add(key_hash)
        if key_hash in own_inputs:
            my_in += prevout["value"]
    if not own_inputs <= spent_hashes:
        problems.append("an input path is not spent by the transaction")
    out_sum = sum(o["value"] for o in tx["outs"])
    my_out = sum(o["value"] for o in tx["outs"] if o["script"] in {p2pkh_script(h) for h in own_outputs})
    fee = in_sum - out_sum
    amount = my_out - my_in
    kind = "credit" if amount > 0 else "internal" if -amount == fee else "debit"
    info = expected["txInfo"]
    if (info["type"], info["amount"], info["fee"]) != (kind, amount, fee):
        problems.append(f"txInfo: expected {kind} {amount} {fee}, vector says {info}")
    return problems


if __name__ == "__main__":
    main()
