#!/bin/bash

set -e

VANILLA=$1
CURRENT_DIR=`pwd`
SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi
SRC_DIR=`cd ${SCRIPTPATH}/.. && pwd`
. "${SCRIPTPATH}/script_include.sh"


if [[ "$OSTYPE" == "darwin"* ]]; then
    USING_OSX=1
fi

mkdir -p ${SRC_DIR}/external

# check the .git of the rjit directory
test -d ${SRC_DIR}/.git
IS_GIT_CHECKOUT=$?

if [ $IS_GIT_CHECKOUT -eq 0 ]; then
    ${SRC_DIR}/tools/install_hooks.sh
fi

function checkout_r {
    NAME=$1
    BRANCH=$2
    R_DIR="${SRC_DIR}/external/${NAME}"

    if [ ! -d $R_DIR ]; then
        cd ${SRC_DIR}/external
        git clone https://github.com/reactorlabs/gnur.git ${NAME}
    fi

    cd $R_DIR

    if [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]; then
        echo "repo is dirty"
        exit 1
    fi

    git fetch
    git checkout origin/$BRANCH
   
    tools/rsync-recommended

    if [ ! -f $R_DIR/Makefile ]; then
        echo "-> configure gnur"
        cd $R_DIR
        if [ $USING_OSX -eq 1 ]; then
          # Mac OSX
            F77="gfortran -arch x86_64" FC="gfortran -arch x86_64" CXXFLAGS="-g3 -O2" CFLAGS="-g3 -O2" ./configure --enable-R-shlib --without-internal-tzcode --with-ICU=no
        else
            CXXFLAGS="-g3 -O2" CFLAGS="-g3 -O2" ./configure --with-ICU=no
        fi
    fi
    
    if [ ! -f $R_DIR/doc/FAQ ]; then
        cd $R_DIR
        touch doc/FAQ
    fi
    if [ ! -f $R_DIR/SVN-REVISION ]; then
        echo "Revision: -99" > SVN-REVISION
        rm -f non-tarball
    fi
}

checkout_r custom-r rir-patch-3-3-branch
if [[ $VANILLA == "--vanilla" ]]; then
    checkout_r vanilla-r R-3-3-branch
fi
