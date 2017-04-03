#!/bin/bash -e

BRANCH=`git branch | grep "*" | tail -c +3`

if [[ "$BRANCH" != "master" ]]; then
    echo "expected to be on master branch"
    exit 1
fi

if [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]; then
    echo "repo is dirty"
    exit 1
fi

SCRIPTPATH=`cd $(dirname "$0") && pwd`
BASE=`cd $SCRIPTPATH/.. && pwd`
TIMEOUT=80
LOOKBACK=5
LOOKBACK_PLOT=50
RUNS=3
REV=`git log -n $LOOKBACK --pretty=format:"%H|%cI"`
BENCH=`ls ${BASE}/benchmarks/shootout/*/*.r`
OUT="benchmark-out/revisions"

mkdir -p $OUT

for revf in $REV; do
    rev=`echo $revf | cut -d'|' -f1`
    dat=`echo $revf | cut -d'|' -f2`
    for run in $(seq 1 $RUNS); do
        log="$OUT/$rev-$run.csv"
        if [ ! -f "$log" ]; then
            echo "**************  testing $run $rev"
            git checkout $rev
            cmake --build . --target clean
            cmake $BASE
            cmake --build . --target setup
            cmake --build .
            for i in $BENCH; do
                T=`basename $i`;
                D="`dirname $i`/../..";
                echo $T
                echo -n "$dat $rev, $T, " >> $log
                R_ENABLE_JIT=2 timeout $TIMEOUT ${SCRIPTPATH}/R -e \
                    "{options(warn=-1); setwd('$D'); source('$i'); execute(); write(system.time(execute())[[3]], stderr())}" \
                      > /dev/null 2>>$log || echo "" >>$log
            done
        fi
    done
done 

git checkout master

REV=`git log -n $LOOKBACK_PLOT --pretty=format:"%H|%cI"`
FINAL=$OUT/benchmark-combined.csv
echo "version, benchmark, time" > $FINAL
for revf in $REV; do
    rev=`echo $revf | cut -d'|' -f1`
    if [[ "`ls $OUT/$rev-*.csv 2>/dev/null`" != "" ]]; then
        cat $OUT/$rev-*.csv >> $FINAL
    fi
done

R -f ${BASE}/tools/benchmark-revisions-plot.r --args $FINAL
