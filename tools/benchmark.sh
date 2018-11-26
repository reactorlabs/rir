#!/bin/bash -e

ARG=$1

SCRIPTPATH=`cd $(dirname "$0") && pwd`
BASE=`cd $SCRIPTPATH/.. && pwd`
PLAIN_R=$BASE/external/vanilla-r/bin/R
MOD_R=$BASE/external/custom-r/bin/R
TIMEOUT=80
OUT="benchmark-out/current"
RUNS=3
 
# checkout gnur without our modifications
$BASE/tools/sync.sh --vanilla
if [ ! -f $PLAIN_R ]; then
    pushd .
    cd $BASE/external/vanilla-r && make -j 8
    popd
fi

if [ "$#" == "1" ]; then
  log=$ARG
else
 
  mkdir -p $OUT
  TIME=`date '+%Y-%m-%d-%H-%M-%S'`
  REV=`git log -n 1 --pretty=format:"%H|%cI"`
  
  log=$OUT/benchmark-$REV-$TIME.csv
  
  echo "experiment, benchmark, time" > $log

  for run in $(seq 1 $RUNS); do
      cmake --build . --target clean
      cmake $BASE
      cmake --build .
  
      for i in `ls ${BASE}/benchmarks/shootout/*/*.r`; do
          echo $T
          T=`basename $i`;
          D="`dirname $i`/../..";
          echo -n "1 R_ENABLE_JIT=2 custom-r, $T, " >> $log;
          R_ENABLE_JIT=2 timeout $TIMEOUT ${MOD_R}  -e \
              "{options(warn=-1); setwd('$D'); source('$i'); execute(); write(system.time(execute())[[3]], stderr())}" \
              > /dev/null 2>>$log || echo "" >>$log
          echo -n "2 R_ENABLE_JIT=0 custom-r, $T, " >> $log;
          R_ENABLE_JIT=0 timeout $TIMEOUT ${MOD_R}  -e \
              "{options(warn=-1); setwd('$D'); source('$i'); execute(); write(system.time(execute())[[3]], stderr())}" \
              > /dev/null 2>>$log || echo "" >>$log
          echo -n "3 R_ENABLE_JIT=2 vanilla-r, $T, " >> $log;
          R_ENABLE_JIT=2 timeout $TIMEOUT $PLAIN_R  -e \
              "{options(warn=-1); setwd('$D'); source('$i'); execute(); write(system.time(execute())[[3]], stderr())}" \
              > /dev/null 2>>$log || echo "" >>$log
          echo -n "4 R_ENABLE_JIT=2 rir, $T, " >> $log;
          R_ENABLE_JIT=2 timeout $TIMEOUT ${SCRIPTPATH}/R -e \
              "{options(warn=-1); setwd('$D'); source('$i'); execute(); write(system.time(execute())[[3]], stderr())}" \
              > /dev/null 2>>$log || echo "" >>$log
      done 
  done
fi

R -f ${BASE}/tools/benchmark-plot.r --args $log
