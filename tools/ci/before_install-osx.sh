#! /bin/bash

echo
echo Running before_install-osx.sh...
echo

brew update-reset
brew install gcc ccache
brew link --overwrite gcc

export PATH="/usr/local/opt/ccache/libexec:$PATH"
