#!/bin/bash -e

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

. "${SCRIPTPATH}/../.local.config"
. "${SCRIPTPATH}/script_include.sh"

export R="${R_HOME}/bin/R"

cd "${SCRIPTPATH}/.."

export R_LIBS_USER=./packages
export R_ENABLE_JIT=3

TESTS=(
    ${R_HOME}/tests/any-all.R
    ${R_HOME}/tests/arith.R
    ${R_HOME}/tests/array-subset.R
    ${R_HOME}/tests/complex.R
)

function run_test {
    echo "--> $0 started"
    $R --slave -f $0 &> /dev/zero
    if [ $? -ne 0 ]; then
        echo "!-> $0 failed!!"
        exit 255
    fi
    echo "--> $0 done"
}
export -f run_test

echo ${TESTS[@]} | xargs -n 1 -P `ncores` bash -c run_test $@

unset R_LIBS_USER
unset R_ENABLE_JIT
