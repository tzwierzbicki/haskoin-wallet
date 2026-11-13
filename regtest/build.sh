#!/usr/bin/env bash
# Build the regtest images of haskoin-wallet.
#
#   regtest/build.sh             both images
#   regtest/build.sh services    the image of the services alone
#
# The tests of Haskoin.Wallet.Regtest need the image of the services, and
# run the hw of the build against it. The other image has hw inside, built
# from the working tree, and stands alone.
#
# 1. verify.sh downloads Ubuntu Base, Bitcoin Core and Bitcoin Cash Node,
#    and checks their signatures. A failure stops the build.
# 2. For the image with hw, the files of this repository that git tracks,
#    or would track, are copied to hw-src/ as they are in the working
#    tree. So the image holds the hw of the working tree, with its changes
#    that are not committed, and none of its build folders.
# 3. podman builds the images from the Containerfile:
#      localhost/hw-regtest-services  the nodes, the stores and the proxy
#      localhost/hw-regtest           the same, with hw inside
#    Each gets the tag "latest". The first carries a label with a hash of
#    its recipe, which the tests read to tell an image built from an older
#    recipe. The second also gets a tag named after the commit of hw.
#
# The first build compiles haskoin-store, which takes most of an hour.
# Podman keeps every step, so the next build runs only the steps that
# changed. A change of the source of hw compiles hw alone.
#
# Use an installed podman. Its containers run under the AppArmor profile
# of podman.
#
# Environment:
#   PODMAN        the podman to run (default: podman)
#   JOBS          packages compiled at once (default: 3)
#   BUILD_MEMORY  the most memory the build may take (default: 10g)
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
HW=$(cd "$HERE/.." && pwd)
PODMAN=${PODMAN:-podman}
WHAT=${1:-all}
case "$WHAT" in
  all | services) ;;
  *) echo "Usage: $0 [services]" >&2; exit 2 ;;
esac

"$HERE/verify.sh"

# A hash of the recipe of the services: the Containerfile, and every file
# that goes into the image. The tests make the same hash from the same
# files, in the same order, and compare.
recipe=$(cd "$HERE" && { cat Containerfile; find rootfs -type f | LC_ALL=C sort | xargs cat; } | sha256sum | cut -c1-16)

build() {
  "$PODMAN" build \
    --memory "${BUILD_MEMORY:-10g}" \
    --build-arg JOBS="${JOBS:-3}" \
    --file "$HERE/Containerfile" \
    "$@" \
    "$HERE"
}

# The stage of hw copies hw-src/, so the folder is there for both builds
mkdir -p "$HERE/hw-src"

build --target services --label "org.haskoin.regtest.recipe=$recipe" --tag localhost/hw-regtest-services:latest
echo "Built localhost/hw-regtest-services:latest, recipe $recipe"
[ "$WHAT" = services ] && exit 0

commit=$(git -C "$HW" rev-parse HEAD)
if [ -n "$(git -C "$HW" status --porcelain)" ]; then
  commit="$commit-changed"
fi

# The source of hw, without what git ignores. The folder is made anew, so
# that a file deleted from the repository leaves the image too.
src=$HERE/hw-src
find "$src" -mindepth 1 -delete
git -C "$HW" ls-files --cached --others --exclude-standard -z \
  | tar -C "$HW" --null --files-from=- -cf - \
  | tar -C "$src" -xf -

build --target regtest --build-arg HW_COMMIT="$commit" \
  --tag localhost/hw-regtest:latest --tag "localhost/hw-regtest:hw-${commit:0:12}"

"$PODMAN" run --rm --entrypoint cat localhost/hw-regtest:latest /etc/regtest-versions
