#! /bin/bash

echo
echo Running before_install-osx.sh...
echo

echo Installing python3
brew install python3 >> brew.log

echo Installing doxygen
brew install doxygen >> brew.log

echo Installing ninja
brew install ninja >> brew.log
