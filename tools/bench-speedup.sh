#!/bin/bash -e


# cwd
WD=`pwd`


# move to rir home
SCRIPTPATH=`cd $(dirname "$0") && pwd`
BASE=`cd $SCRIPTPATH/.. && pwd`

pushd $BASE/build


# checks and setup
if [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]; then
    echo "repo is dirty"
    popd
    exit 1
fi

if BRANCH=`git symbolic-ref --short -q HEAD`
then
    echo "on branch $BRANCH"
else
    echo "detached head"
    popd
    exit 1
fi

RIR_REVISION=`git rev-parse HEAD`
if [ "$#" == "1" ]; then
    RIR_REVISION=$1
fi
PLAIN_R=$BASE/external/vanilla-r/bin/R
MOD_R=$BASE/external/custom-r/bin/R
BENCH=`ls ${BASE}/benchmarks/shootout/*/*.r`
REV=`git log $RIR_REVISION -n 1 --pretty=format:"%h"`
TIMEOUT=80
RUNS=3
OUT="$WD/benchmark-out/`git log $RIR_REVISION -n 1 --pretty=format:'%cd_%h' --date=format:'%Y-%m-%d_%H-%M-%S'`"

mkdir -p $OUT


# checkout and build rir (ensure vanilla-r and custom-r)
git checkout $RIR_REVISION

ninja clean
cmake -GNinja $BASE
ninja setup
ninja

$BASE/tools/sync.sh --vanilla
pushd .
cd $BASE/external/vanilla-r && make -j 8
popd


# run benchmarks
TIME=`date '+%Y-%m-%d-%H-%M-%S'`
log="$OUT/benchmark-$REV-$TIME.csv"
echo "experiment, benchmark, time" > $log
for run in $(seq 1 $RUNS); do
    echo "**************  testing run $run"
    for i in $BENCH; do
        T=`basename $i`;
        D="`dirname $i`/../..";
        echo $T

        echo -n "1 R_ENABLE_JIT=0 vanilla-r, $T, " >> $log;
        R_ENABLE_JIT=0 timeout $TIMEOUT ${PLAIN_R}  -e \
            "{options(warn=-1); setwd('$D'); source('$i'); execute(); write(system.time(execute())[[3]], stderr())}" \
            > /dev/null 2>>$log || echo "" >>$log

        echo -n "2 R_ENABLE_JIT=2 vanilla-r, $T, " >> $log;
        R_ENABLE_JIT=2 timeout $TIMEOUT ${PLAIN_R}  -e \
            "{options(warn=-1); setwd('$D'); source('$i'); execute(); write(system.time(execute())[[3]], stderr())}" \
            > /dev/null 2>>$log || echo "" >>$log
        
        echo -n "3 R_ENABLE_JIT=2 rir, $T, " >> $log;
        R_ENABLE_JIT=2 timeout $TIMEOUT ${SCRIPTPATH}/R -e \
            "{options(warn=-1); setwd('$D'); source('$i'); execute(); write(system.time(execute())[[3]], stderr())}" \
            > /dev/null 2>>$log || echo "" >>$log

    done
done


# return to where we were
git checkout $BRANCH

ninja clean
cmake -GNinja $BASE
ninja setup
ninja

$BASE/tools/sync.sh --vanilla
pushd .
cd $BASE/external/vanilla-r && make -j 8
popd

popd

R -f ${BASE}/tools/bench-speedup-plot.r --args $log
