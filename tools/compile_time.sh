#!/bin/bash -e

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

# Run install script to deploy new hooks
$SCRIPTPATH/install_hooks.sh

. "${SCRIPTPATH}/../.local.config"
. "${SCRIPTPATH}/script_include.sh"

if [ -d "$1" ]; then
    BUILD_DIR=$1
    echo "Using non-default build dir $BUILD_DIR"
fi

STATUS=$(mktemp /tmp/r-test-status.XXXXXX)

COMPILE_JOB="
system.time({
    benv <- environment(base:::xtfrm.default);
    for (name in names(benv)) {
        x = benv[[name]];
        if (typeof(x) == 'closure')
            jit.compile(x)
    }
})"


R="${R_HOME}/bin/R -q --slave"

if test "$(uname)" = "Darwin"; then
    LIB="dyn.load('${BUILD_DIR}/librjit.dylib')"
else
    LIB="dyn.load('${BUILD_DIR}/librjit.so')"
fi

TEST=$(mktemp /tmp/r-test.XXXXXX)
echo ${LIB} > $TEST
echo "source('${ROOT_DIR}/rjit/R/rjit.R')" >> $TEST
echo $COMPILE_JOB >> $TEST

$R -f $TEST
res=$?

rm $TEST
