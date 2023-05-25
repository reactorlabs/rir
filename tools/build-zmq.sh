#!/bin/bash

set -e

SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
if [ ! -d "$SCRIPTPATH" ]; then
    echo "Could not determine absolute dir of $0"
    exit 1
fi
. "${SCRIPTPATH}/script_include.sh"
SRC_DIR="${SCRIPTPATH}/.."
EXTERNAL_DIR="${SRC_DIR}/external"

if [ -d "${EXTERNAL_DIR}/zeromq" ] && [ -n "${FORCE}" ]; then
    echo "-> removing old zeromq build..."
    rm -rf "${EXTERNAL_DIR}/zeromq"
fi

if [ ! -d "${EXTERNAL_DIR}/zeromq" ]; then
    echo "-> building zeromq..."
    # https://github.com/... is the path to the zeromq source release
    wget -qO- https://github.com/zeromq/libzmq/releases/download/v4.3.4/zeromq-4.3.4.tar.gz | tar -xz -C "${EXTERNAL_DIR}"
    cd "${EXTERNAL_DIR}/zeromq-4.3.4"
    # --disable-Werror must be passed because of https://github.com/zeromq/libzmq/issues/4391, which is still open :(
    ./configure --prefix="${EXTERNAL_DIR}/zeromq" --enable-debug --disable-Werror && make -j "$(ncores)" && make install
else
    echo "-> zeromq already built, run with FORCE=1 to force rebuild. Skipping..."
fi