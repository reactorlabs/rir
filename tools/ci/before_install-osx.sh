#! /bin/bash

echo
echo Running before_install-osx.sh...
echo

brew install gcc
brew link --overwrite gcc

brew install xz
#brew install valgrind
