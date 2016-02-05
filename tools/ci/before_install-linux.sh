#! /bin/bash

echo
echo Running before_install-linux.sh...
echo

echo Installing cmake
mkdir cmake && wget --no-check-certificate https://cmake.org/files/v3.3/cmake-3.3.0-Linux-x86_64.tar.gz
tar -zxf cmake-3.3.0-Linux-x86_64.tar.gz --strip-components=1 -C cmake
export PATH=$TRAVIS_BUILD_DIR/cmake/bin:${PATH}
