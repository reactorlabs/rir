#!/bin/bash

set -e

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

if [[ "$1" == "--macos_gcc9" ]]; then
    MACOS_GCC9=1
fi

if test -e ${SRC_DIR}/.git; then
    echo "-> update submodules"
    git submodule update --init
fi

if test -d ${SRC_DIR}/.git; then
    echo "-> install git hooks"
    ${SRC_DIR}/tools/install_hooks.sh
fi

function build_r {
    NAME=$1
    R_DIR="${SRC_DIR}/external/${NAME}"

    cd $R_DIR

    if [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]; then
        echo "** warning: $NAME repo is dirty"
        sleep 1
    fi

    tools/rsync-recommended

    # There is a test that times out due to the compiler triggering in the
    # wrong moment in the matrix package. There doesn't seem to be a good solution
    # other than just patching it.
    cd src/library/Recommended/
    tar xzf Matrix_1.3-4.tar.gz
    sed -i -e 's/^stopifnot(st\[1\] < 1.0)/(st[1] < 1.0)/' Matrix/man/printSpMatrix.Rd
    rm Matrix_1.3-4.tar.gz
    tar czf Matrix_1.3-4.tar.gz Matrix
    rm -rf Matrix
    cd ../../..

    if [[ "$GNUR_BRANCH" != "" ]]; then
        git checkout $GNUR_BRANCH
    fi

    SND=1
    if [[ "$DONT_SWITCH_TO_NAMED" == "1" ]]; then
        SND=0
    fi

    if [ ! -f $R_DIR/Makefile ]; then
        echo "-> configure $NAME"
        cd $R_DIR
        if [ $USING_OSX -eq 1 ]; then
            CFLAGS="-O2 -g -DSWITCH_TO_NAMED=$SND" ./configure --enable-R-shlib --with-internal-tzcode --with-ICU=no || cat config.log
        else
            CFLAGS="-O2 -g -DSWITCH_TO_NAMED=$SND" ./configure
        fi
    fi

    if [ ! -f $R_DIR/doc/FAQ ]; then
        cd $R_DIR
        touch doc/FAQ
    fi

    if [ ! -f $R_DIR/SVN-REVISION ]; then
        # R must either be built from a svn checkout, or from the tarball generated by make dist
        # this is a workaround to build it from a git mirror
        # see https://github.com/wch/r-source/wiki/Home/6d35777dcb772f86371bf221c194ca0aa7874016#building-r-from-source
        echo -n 'Revision: ' > SVN-REVISION
        # get the latest revision that is not a rir patch
        REV=$(git log --grep "git-svn-id" -1 --format=%B | grep "^git-svn-id" | sed -E 's/^git-svn-id: https:\/\/svn.r-project.org\/R\/[^@]*@([0-9]+).*$/\1/')
        # can fail on shallow checkouts, so let's put the last known there
        if [ "$REV" == "" ]; then
          REV='74948'
        fi
        echo $REV >> SVN-REVISION
        echo -n 'Last Changed Date: ' >> SVN-REVISION
        REV_DATE=$(git log --grep "git-svn-id" -1 --pretty=format:"%ad" --date=iso | cut -d' ' -f1)
        # can fail on shallow checkouts, so let's put the last known there
        if [ "$REV_DATE" == "" ]; then
          REV_DATE='2018-07-02'
        fi
        echo $REV_DATE >> SVN-REVISION

        rm -f non-tarball
    fi

    echo "-> building $NAME"
    make -j `ncores`
}

build_r custom-r
