#!/bin/bash

set -e

CURRENT_DIR=`pwd`
SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi
SRC_DIR=`cd ${SCRIPTPATH}/.. && pwd`
. "${SCRIPTPATH}/script_include.sh"

if [ -d "${SRC_DIR}/external/llvm-8.0.0" ]; then
    echo "${SRC_DIR}/external/llvm-8.0.0 already exists. Remove it to install a debug version of llvm."
    exit 1
fi

cd "${SRC_DIR}/external"

if [ ! -f llvm-8.0.0.src.tar.xz ]; then
    wget http://releases.llvm.org/8.0.0/llvm-8.0.0.src.tar.xz
fi
if [ ! -d "llvm-8.0.0.src" ]; then
    tar xf llvm-8.0.0.src.tar.xz
    mkdir llvm-8.0.0
    cd llvm-8.0.0.src
    mkdir build
    cd build
    cmake -DCMAKE_BUILD_TYPE=Debug -GNinja ..
    ninja
else
    cd llvm-8.0.0.src/build
fi
cmake -DCMAKE_INSTALL_PREFIX=../../llvm-8.0.0 -P cmake_install.cmake
