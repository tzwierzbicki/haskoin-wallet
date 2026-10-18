#!/usr/bin/env python3
"""Verify the message signing test vectors with Bitcoin Core's verifymessage.

Usage: verify_with_bitcoin_core.py BITCOIN_BIN_DIR VECTOR_DIR

  BITCOIN_BIN_DIR  directory containing bitcoind and bitcoin-cli

Starts a throwaway regtest node in a temporary directory (no wallet, no
network connections, RPC on localhost only) and stops it at the end. Regtest
only accepts regtest addresses, so each vector's address is converted to the
regtest P2PKH address with the same public key hash. Messages are sent over
JSON-RPC, so they arrive byte for byte (newlines, NUL bytes, long messages).
"""

import base64
import json
import os
import socket
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request

import btc


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    bin_dir, directory = sys.argv[1], sys.argv[2]
    with tempfile.TemporaryDirectory() as datadir:
        port = free_port()
        args = [f"-datadir={datadir}", "-regtest", f"-rpcport={port}"]
        subprocess.run([os.path.join(bin_dir, "bitcoind"), *args, "-daemon", "-listen=0", "-connect=0",
                        "-dnsseed=0", "-discover=0", "-disablewallet", "-server", "-rpcbind=127.0.0.1",
                        "-rpcallowip=127.0.0.1"], check=True, capture_output=True)
        try:
            subprocess.run([os.path.join(bin_dir, "bitcoin-cli"), *args, "-rpcwait", "-rpcwaittimeout=60",
                            "getblockchaininfo"], check=True, capture_output=True)
            rpc = RPC(port, os.path.join(datadir, "regtest", ".cookie"))
            print("Bitcoin Core", rpc.call("getnetworkinfo")["subversion"])
            failures = verify_all(rpc, directory)
        finally:
            subprocess.run([os.path.join(bin_dir, "bitcoin-cli"), *args, "stop"], capture_output=True)
            wait_for_shutdown(datadir)
    for failure in failures:
        print("  " + failure)
    sys.exit(1 if failures else 0)


def verify_all(rpc, directory):
    failures = []
    for filename in sorted(f for f in os.listdir(directory) if f.endswith(".json")):
        with open(os.path.join(directory, filename), encoding="utf-8") as f:
            vectors = json.load(f).get("vectors", [])
        file_failures = []
        for v in vectors:
            regtest_address = btc.base58check_encode(b"\x6f" + btc.p2pkh_hash(v["address"]))
            message = v["message"] * v.get("messageRepeat", 1)
            valid = rpc.call("verifymessage", regtest_address, v["signature"], message)
            # "differ" vectors sign other bytes than the message: Core must reject them
            expected = v.get("expect", "match") != "differ"
            if valid is not expected:
                file_failures.append(f"{filename}: {v['name']}: verifymessage returned {valid}")
        print(f"{filename}: {len(vectors)} vectors, {'ok' if not file_failures else 'FAILED'}")
        failures += file_failures
    return failures


class RPC:
    def __init__(self, port, cookie_file):
        with open(cookie_file) as f:
            self.auth = "Basic " + base64.b64encode(f.read().strip().encode()).decode()
        self.url = f"http://127.0.0.1:{port}/"

    def call(self, method, *params):
        body = json.dumps({"jsonrpc": "1.0", "id": 1, "method": method, "params": list(params)}).encode()
        request = urllib.request.Request(self.url, body, {"Authorization": self.auth,
                                                          "Content-Type": "application/json"})
        try:
            return json.load(urllib.request.urlopen(request))["result"]
        except urllib.error.HTTPError as e:
            return "error: " + json.load(e)["error"]["message"]


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def wait_for_shutdown(datadir):
    """Wait until bitcoind has released its data directory"""
    pid_file = os.path.join(datadir, "regtest", "bitcoind.pid")
    for _ in range(100):
        if not os.path.exists(pid_file):
            return
        time.sleep(0.1)


if __name__ == "__main__":
    main()
