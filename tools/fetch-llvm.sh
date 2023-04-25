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


if [[ "$OSTYPE" == "darwin"* ]]; then
    USING_OSX=1
fi

LLVM_DIR="${SRC_DIR}/external/llvm-12"
if [ ! -d $LLVM_DIR ]; then
    echo "-> unpacking LLVM"
    cd "${SRC_DIR}/external"
    if [ $USING_OSX -eq 1 ]; then
        F="clang+llvm-12.0.0-x86_64-apple-darwin"
        if [ ! -f "$F.tar.xz" ]; then
            curl -L https://github.com/llvm/llvm-project/releases/download/llvmorg-12.0.0/$F.tar.xz > $F.tar.xz
        fi
        tar xf $F.tar.xz
        ln -s $F llvm-12
    else
        V=`grep DISTRIB_RELEASE /etc/lsb-release | cut -d= -f2`
        if [ "$V" == "18.04" ]; then
          V="16.04"
        fi
        if [ "$V" == "20.10" ]; then
          V="20.04"
        fi
        if [ "$V" == "22.04" ]; then
          V="20.04"
        fi
        if [ "$BUILD_LLVM_FROM_SRC" == "1" ]; then
          V=""
        fi
        if [ "$V" == "20.10" ] || [ "$V" == "20.04" ] || [ "$V" == "16.04" ]; then
          MINOR="0"
          F="clang+llvm-12.0.$MINOR-x86_64-linux-gnu-ubuntu-$V"
          if [ ! -f "$F.tar.xz" ]; then
              curl -L https://github.com/llvm/llvm-project/releases/download/llvmorg-12.0.$MINOR/$F.tar.xz > $F.tar.xz
          fi
          tar xf $F.tar.xz
          ln -s $F llvm-12
        else
          F="llvm-12.0.0.src"
          if [ ! -f "$F.tar.xz" ]; then
            curl -L https://github.com/llvm/llvm-project/releases/download/llvmorg-12.0.0/$F.tar.xz > $F.tar.xz
          fi
          tar xf $F.tar.xz
          mkdir llvm-12-build && cd llvm-12-build
          cmake -GNinja -DCMAKE_BUILD_TYPE=RelWithDebInfo -DLLVM_ENABLE_ASSERTIONS=1 -DLLVM_OPTIMIZED_TABLEGEN=1 -DLLVM_USE_PERF=1 -DLLVM_TARGETS_TO_BUILD="X86" ../$F
          ninja
          cd ..
          ln -s llvm-12-build llvm-12
        fi
    fi
fi
