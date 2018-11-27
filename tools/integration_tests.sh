#!/bin/sh -e

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

. "${SCRIPTPATH}/../.local.config"

export R="${R_HOME}/bin/R"

cd "${SCRIPTPATH}/.."

cmake --build $BUILD_DIR --target tests

echo "==> Testing R CMD check"
cmake --build $BUILD_DIR --target package_check

echo "==> Installing package"
cmake --build $BUILD_DIR --target package_install

echo ""
echo "==> Running selected gnur tests"
echo "    skipped"
tools/gnur_tests.sh

echo ""
echo "==> Running testr samples"
tools/testr_tests.sh
# save success result for CI
echo "INTEGRATION_TESTS_SUCCESS=1" >> "${SCRIPTPATH}/../.test_results"
