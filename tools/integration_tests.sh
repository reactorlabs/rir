#!/bin/sh -e

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

. "${SCRIPTPATH}/../.local.config"

export R="${R_HOME}/bin/R"

cd "${SCRIPTPATH}/.."

echo "==> Testing R CMD check"
time -p cmake --build $BUILD_DIR --target package_check

echo ""
echo "==> Running selected gnur tests"
time -p tools/gnur_tests.sh

echo ""
echo "==> Running testr samples"
time -p tools/testr_tests.sh
