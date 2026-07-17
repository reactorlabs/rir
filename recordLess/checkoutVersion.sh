#!/bin/bash
set -e

git checkout $1

# update and build gnu-R
git submodule update


cd ..
cd external/custom-r
make
