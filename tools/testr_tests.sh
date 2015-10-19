#!/bin/sh -e

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

. "${SCRIPTPATH}/../.local.config"

cd "${SCRIPTPATH}/.."

tools/testr $TESTR_DIR/filtered-test-suite 2>&1 | tee /tmp/testr.out
