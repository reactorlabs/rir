#!/bin/bash

CURRENT_DIR=`pwd`
SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
    exit 1
fi

WHICH="$SCRIPTPATH/../external/$1"
if [ ! -d $WHICH ]; then
    echo "no such R $WHICH"
    exit 1
fi

cd $WHICH
make -j8

# sometimes package install fails due to mysteryous locking issues...
rm -rf library/*LOCK-*
make -j8
