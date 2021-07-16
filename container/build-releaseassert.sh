#!/bin/sh

curl 10.200.14.25:8080/clang+llvm-12.0.0-x86_64-linux-gnu-ubuntu-20.04.tar.xz > /opt/rir/external/clang+llvm-12.0.0-x86_64-linux-gnu-ubuntu-20.04.tar.xz
/opt/rir/tools/fetch-llvm.sh
mkdir /opt/rir/build/releaseassert
cd /opt/rir/build/releaseassert
cmake -DCMAKE_BUILD_TYPE=RELEASESLOWASSERT ../.. && make -j6
