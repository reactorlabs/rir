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

if [ -d "${SRC_DIR}/external/llvm-12" ]; then
    echo "${SRC_DIR}/external/llvm-12 already exists. Remove it to install a debug version of llvm."
    exit 1
fi

cd "${SRC_DIR}/external"

if [ ! -f llvm-12.0.0.src.tar.xz ]; then
    wget https://github.com/llvm/llvm-project/releases/download/llvmorg-12.0.0/llvm-12.0.0.src.tar.xz
fi
if [ ! -d "llvm-12.0.0.src" ]; then
    tar xf llvm-12.0.0.src.tar.xz
    mkdir llvm-12
    cd llvm-12.0.0.src
    mkdir build
    cd build
    cmake -DCMAKE_BUILD_TYPE=Debug -GNinja ..
    ninja
else
    cd llvm-12.0.0.src/build
fi
cmake -DCMAKE_INSTALL_PREFIX=../../llvm-12 -P cmake_install.cmake
