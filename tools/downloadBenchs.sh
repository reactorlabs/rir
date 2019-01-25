#!/usr/bin/env bash
set -e

SCRIPT_PATH="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"
if [ ! -d $SCRIPT_PATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

## First download the benchmarks from the RBenchmarking repo
## in sparse-checkout mode to avoid bringing the whole repo

ROOT_PATH="$SCRIPT_PATH/.."
BENCHMARKS_PATH="$ROOT_PATH/benchmarks"

REPO_NAME="RBenchmarking"
REPO_URL="https://github.com/reactorlabs/rbenchmarking"
REPO_PATH="$BENCHMARKS_PATH/$REPO_NAME"

if [ ! -d "$BENCHMARKS_PATH" ]
then
    mkdir "$BENCHMARKS_PATH"
fi

pushd "$BENCHMARKS_PATH"

if [ ! -d "$REPO_PATH" ]
  then
    mkdir "$REPO_PATH"
    pushd "$REPO_PATH"
    git init
    git config core.sparseCheckout true
    git remote add -f origin $REPO_URL
    echo "Benchmarks" > .git/info/sparse-checkout
    echo "rebench.conf" >> .git/info/sparse-checkout
    popd > /dev/null
fi

## Copy the needed directories and files to the root benchmarks dir
## and cleanup everything else
pushd "$REPO_PATH"
git pull --depth=1 origin master
for D in Benchmarks/*; do
    rm -Rf "$BENCHMARKS_PATH/$(basename $D)"
done
mv -f Benchmarks/* "$BENCHMARKS_PATH/"
mv -f rebench.conf "$BENCHMARKS_PATH/"
popd > /dev/null

rm -Rf $REPO_PATH

if [ "$1" == "--travis" ]
then
    PREFIX="build\/"
fi

## Customize the locations of RIR, GNU-R and the benchmarks in
## rebench's conf file
sed -i.bak 's/\&LOCATION_AWF .*$/\&LOCATION_AWF "areWeFast"/' "$BENCHMARKS_PATH/rebench.conf"
sed -i.bak 's/\&LOCATION_SHT .*$/\&LOCATION_SHT "shootout"/' "$BENCHMARKS_PATH/rebench.conf"
sed -i.bak 's/\&LOCATION_RIR .*$/\&LOCATION_RIR "\.\.\/'"$PREFIX"'bin"/' "$BENCHMARKS_PATH/rebench.conf"
sed -i.bak '/warmup:/d' "$BENCHMARKS_PATH/rebench.conf"
rm "$BENCHMARKS_PATH/rebench.conf.bak"

popd > /dev/null

