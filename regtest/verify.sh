#!/usr/bin/env bash
# Download what the image is made of, and check every signature.
#
#   ./verify.sh
#
# It fills the folder downloads/ beside this script, and stops at the first
# file that does not check. Run it before every build: build.sh does.
#
# What is trusted, and why:
#
# - Ubuntu Base: the list of hashes is signed by the "Ubuntu CD Image
#   Automatic Signing Key (2012)". The key is read from the keyring that
#   the Ubuntu of this computer installed, so no key is downloaded.
# - Bitcoin Core: the list of hashes is signed by the builders of the
#   release. Their keys come from the repository bitcoin-core/guix.sigs.
#   A downloaded key counts only when its fingerprint is one of those
#   written below, and at least CORE_SIGNERS_NEEDED of them must have
#   signed.
# - Bitcoin Cash Node: each maintainer signs a copy of the list of hashes.
#   Their keys come from contrib/gitian-signing of the project, and count
#   only by the fingerprints written below. Both must have signed.
#
# The hashes written below are a second lock. A release that is signed but
# is not the one this recipe was written for stops the build too.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
DL=$HERE/downloads

UBUNTU_RELEASE=26.04
UBUNTU_TARBALL=ubuntu-base-26.04.1-base-amd64.tar.gz
UBUNTU_SHA256=a496a960472ce474a59590b8987d3a1135d3cbef1991f3b1abe8cacfea8bf85a
UBUNTU_KEYRING=/usr/share/keyrings/ubuntu-archive-keyring.gpg
UBUNTU_KEY=843938DF228D22F7B3742BC0D94AA3F0EFE21092

CORE_VERSION=32.1
CORE_TARBALL=bitcoin-$CORE_VERSION-x86_64-linux-gnu.tar.gz
CORE_SHA256=61545c17c3120fb7412f896d248b11993af04ec8817aad3649c55b2afb044963
CORE_SIGNERS_NEEDED=5
# name in guix.sigs/builder-keys, and the fingerprint of the primary key
CORE_BUILDERS="
achow101      152812300785C96444D3334D17565732E08E5E41
benthecarman  0AD83877C1F0CD1EE9BD660AD7CC770B81FD22A8
fanquake      E777299FC265DD04793070EB944D35F9AC3DB76A
guggero       F4FC70F07310028424EFC20A8E4256593F177720
pinheadmz     E61773CD6E01040E2F1BD78CE7E2984B6289C93A
sedited       A8FC55F3B04BA3146F3492E79303B33A305224CB
Sjors         ED9BDF7AD6A55E232E84524257FF9BDBCC301009
willcl-ark    67AA5B46E7AF78053167FE343B8F814A784218F8
"

BCHN_VERSION=29.2.0
BCHN_TARBALL=bitcoin-cash-node-$BCHN_VERSION-x86_64-linux-gnu.tar.gz
BCHN_SHA256=23bbe9ff749136a9b832a2a59023ce76a5391322c1ea2f4f24b5b91f035081bc
# name of the signed file in announcements/release-sigs, and the
# fingerprint of the primary key
BCHN_MAINTAINERS="
Calin_Culianu       D465135F97D0047E18E99DC321810A542031C02C
imaginary_username  BA578BC691D53253036D02C8A93E50646A20ED95
"

say() { printf '%s\n' "$*"; }
die() { printf 'FAILED: %s\n' "$*" >&2; exit 1; }

# fetch URL FILE: download once, and keep the file for the next run
fetch() {
  [ -s "$2" ] && return 0
  mkdir -p "$(dirname "$2")"
  curl -fsSL --retry 3 -o "$2.part" "$1" || die "could not download $1"
  mv "$2.part" "$2"
}

# check_hash FILE SHA256
check_hash() {
  local got
  got=$(sha256sum "$1" | cut -d' ' -f1)
  [ "$got" = "$2" ] || die "$(basename "$1") has the hash $got, and $2 is expected"
}

# signers KEYRING_HOME SIGNATURE [SIGNED_FILE]: the fingerprint of the
# primary key of every good signature, one per line
signers() {
  local home=$1; shift
  GNUPGHOME=$home gpg --batch --status-fd 1 --verify "$@" 2>/dev/null \
    | awk '$2 == "VALIDSIG" { print $NF }' | sort -u
}

# a keyring of its own for each project, thrown away at the end
KEYS=$(mktemp -d)
trap 'rm -rf "$KEYS"' EXIT

say "== Ubuntu Base $UBUNTU_RELEASE"
U=$DL/ubuntu-base
for f in SHA256SUMS SHA256SUMS.gpg "$UBUNTU_TARBALL"; do
  fetch "https://cdimage.ubuntu.com/ubuntu-base/releases/$UBUNTU_RELEASE/release/$f" "$U/$f"
done
[ -r "$UBUNTU_KEYRING" ] || die "no keyring at $UBUNTU_KEYRING: install the package ubuntu-keyring"
mkdir -m 700 "$KEYS/ubuntu"
good=$(GNUPGHOME=$KEYS/ubuntu gpg --batch --no-default-keyring --keyring "$UBUNTU_KEYRING" \
  --status-fd 1 --verify "$U/SHA256SUMS.gpg" "$U/SHA256SUMS" 2>/dev/null | awk '$2 == "VALIDSIG" { print $NF }')
[ "$good" = "$UBUNTU_KEY" ] || die "SHA256SUMS of Ubuntu Base is not signed by $UBUNTU_KEY"
grep -q "^$UBUNTU_SHA256 \*$UBUNTU_TARBALL\$" "$U/SHA256SUMS" || die "the signed list does not give $UBUNTU_SHA256 for $UBUNTU_TARBALL"
check_hash "$U/$UBUNTU_TARBALL" "$UBUNTU_SHA256"
say "   signed by $UBUNTU_KEY, the CD image key of the keyring of this computer"
say "   $UBUNTU_TARBALL  $UBUNTU_SHA256"

say "== Bitcoin Core $CORE_VERSION"
C=$DL/bitcoin-core
for f in SHA256SUMS SHA256SUMS.asc "$CORE_TARBALL"; do
  fetch "https://bitcoincore.org/bin/bitcoin-core-$CORE_VERSION/$f" "$C/$f"
done
mkdir -m 700 "$KEYS/core"
while read -r name fingerprint; do
  [ -n "$name" ] || continue
  fetch "https://raw.githubusercontent.com/bitcoin-core/guix.sigs/main/builder-keys/$name.gpg" "$DL/bitcoin-core-keys/$name.gpg"
  GNUPGHOME=$KEYS/core gpg --batch --quiet --import "$DL/bitcoin-core-keys/$name.gpg" 2>/dev/null
done <<<"$CORE_BUILDERS"
count=0
for fingerprint in $(signers "$KEYS/core" "$C/SHA256SUMS.asc" "$C/SHA256SUMS"); do
  name=$(awk -v f="$fingerprint" '$2 == f { print $1 }' <<<"$CORE_BUILDERS")
  # a key that is not written above does not count, whatever its name
  [ -n "$name" ] || continue
  say "   signed by $fingerprint  $name"
  count=$((count + 1))
done
[ "$count" -ge "$CORE_SIGNERS_NEEDED" ] || die "$count known builders signed Bitcoin Core $CORE_VERSION, and $CORE_SIGNERS_NEEDED are needed"
grep -q "^$CORE_SHA256  $CORE_TARBALL\$" "$C/SHA256SUMS" || die "the signed list does not give $CORE_SHA256 for $CORE_TARBALL"
check_hash "$C/$CORE_TARBALL" "$CORE_SHA256"
say "   $CORE_TARBALL  $CORE_SHA256"

say "== Bitcoin Cash Node $BCHN_VERSION"
B=$DL/bitcoin-cash-node
fetch "https://github.com/bitcoin-cash-node/bitcoin-cash-node/releases/download/v$BCHN_VERSION/$BCHN_TARBALL" "$B/$BCHN_TARBALL"
fetch "https://gitlab.com/bitcoin-cash-node/bitcoin-cash-node/-/raw/master/contrib/gitian-signing/pubkeys.txt" "$DL/bitcoin-cash-node-keys/pubkeys.txt"
mkdir -m 700 "$KEYS/bchn"
GNUPGHOME=$KEYS/bchn gpg --batch --quiet --import "$DL/bitcoin-cash-node-keys/pubkeys.txt" 2>/dev/null
while read -r who fingerprint; do
  [ -n "$who" ] || continue
  signed=$B/SHA256SUMS.$BCHN_VERSION.asc.$who
  fetch "https://gitlab.com/bitcoin-cash-node/announcements/-/raw/master/release-sigs/$BCHN_VERSION/SHA256SUMS.$BCHN_VERSION.asc.$who" "$signed"
  [ "$(signers "$KEYS/bchn" "$signed")" = "$fingerprint" ] || die "$(basename "$signed") is not signed by $fingerprint"
  # read the hash from the text that the signature covers, not from the file
  GNUPGHOME=$KEYS/bchn gpg --batch --decrypt "$signed" 2>/dev/null | grep -q "^$BCHN_SHA256  $BCHN_TARBALL\$" \
    || die "the list signed by $who does not give $BCHN_SHA256 for $BCHN_TARBALL"
  say "   signed by $fingerprint  $who"
done <<<"$BCHN_MAINTAINERS"
check_hash "$B/$BCHN_TARBALL" "$BCHN_SHA256"
say "   $BCHN_TARBALL  $BCHN_SHA256"

say "All signatures and hashes are good."
