#!/bin/sh

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

. "${SCRIPTPATH}/../.local.config"

cd "${SCRIPTPATH}/.."

tools/testr $TESTR_DIR/filtered-test-suite 2>&1 | tee /tmp/testr.out

NUM_FAILED=`tail -n 1 /tmp/testr.out | sed 's/\([0-9]*\).*/\1/'`

if [ $NUM_FAILED -gt 25 ]; then
    echo "more than 25 testr failures"
    exit 1;
fi

exit 0
