#!/usr/bin/env bash
set -e

SCRIPT_PATH="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"
if [ ! -d $SCRIPT_PATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

if [[ "$1" == "--travis" || "$2" == "--travis" ]]
then
    OPTIONS="e:RIR e:PIR -r"
fi

if [ "$1" == "--installRebench" ]
then
    ## We use a patched version that supports the usage of env variables
    ## in commands. Otherwise pip install --user ReBench
    git clone https://github.com/charig/ReBench.git -b envVarsSupport
    pushd ReBench > /dev/null
    pip install --user .
    popd > /dev/null
fi


export PATH=$HOME/.local/bin:$PATH
pushd "$SCRIPT_PATH/../benchmarks"
rebench rebench.conf $OPTIONS -SN --iterations=2
popd > /dev/null
