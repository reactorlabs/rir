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
    rm -rf "${EXTERNAL_DIR}/zeromq" "${EXTERNAL_DIR}/zeromq-4.3.4" "${EXTERNAL_DIR}/cppzmq-4.9.0"
fi

if [ ! -d "${EXTERNAL_DIR}/zeromq" ]; then
    echo "-> building libzmq..."
    # https://github.com/... is the path to the libzmq source release
    wget -qO- https://github.com/zeromq/libzmq/releases/download/v4.3.4/zeromq-4.3.4.tar.gz | tar -xz -C "${EXTERNAL_DIR}"
    cd "${EXTERNAL_DIR}/zeromq-4.3.4"
    # --disable-Werror must be passed because of https://github.com/zeromq/libzmq/issues/4391, which is still open :(
    ./configure --prefix="${EXTERNAL_DIR}/zeromq" --enable-debug --disable-Werror && make -j "$(ncores)" && make install
    echo "-> building cppzmq"
      # https://github.com/... is the path to the cppzmq source release
    wget -qO- https://github.com/zeromq/cppzmq/archive/refs/tags/v4.9.0.tar.gz | tar -xz -C "${EXTERNAL_DIR}"
    cd "${EXTERNAL_DIR}/cppzmq-4.9.0"
    if [ "${USE_NINJA}" -eq 0 ]; then
      GNINJA="-GNinja"
    else
      GNINJA=""
    fi
    # TODO: Switch to release if CMake is in a release configuration
    cmake "${GNINJA}" -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX="${EXTERNAL_DIR}/zeromq" -B build && cmake --build build --target install
    # We don't enable exceptions. cppzmq throws exceptions. There isn't really a good alternative API.
    # What do we do? Replace all `throw ...` with `zeromq_error()`. Right now this aborts; in the future, if we actually
    # want to handle exceptions, we can use longjmp (poor man's exception)
    echo "-> patching cppzmq..."
    sub() {
        sed -i.bak "s/$1/$2/g" "${EXTERNAL_DIR}/zeromq/include/zmq.hpp" "${EXTERNAL_DIR}/zeromq/include/zmq_addon.hpp"
        rm "${EXTERNAL_DIR}/zeromq/include/zmq.hpp.bak" "${EXTERNAL_DIR}/zeromq/include/zmq_addon.hpp.bak"
    }
    sub "throw error_t();" "rir::zeromq_error(__func__);"
    sub "throw std::exception();" "rir::zeromq_error(__func__);"
else
    echo "-> zeromq already built, run with FORCE=1 to force rebuild. Skipping..."
fi
