#! /bin/bash

echo
echo Running before_install-osx.sh...
echo

brew update

echo Installing cmake
brew install cmake

git clone https://github.com/RomanTsegelskyi/llvm llvm
