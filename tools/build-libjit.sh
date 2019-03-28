#!/bin/bash

CURRENT_DIR=`pwd`
SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
    exit 1
fi

WHICH="$SCRIPTPATH/../external/libjit"
if [ ! -d $WHICH ]; then
    echo "no such R $WHICH"
    exit 1
fi

cd $WHICH
./bootstrap
CFLAGS=-fPIC CPPFLAGS=-fPIC ./configure
make -j8
