#!/bin/bash -e

ARG=$1

SCRIPTPATH=`cd $(dirname "$0") && pwd`
BASE=`cd $SCRIPTPATH/.. && pwd`
PLAIN_R=~/Documents/freshr/R-3-2-branch/bin/R
TIMEOUT=30
OUT="benchmark-out/current"
RUNS=3
 
if [ ! -f $PLAIN_R ]; then
    echo "expected plain R at $PLAIN_R"
    exit 1
fi

if [ "$#" == "1" ]; then
  log=$ARG
else
 
  mkdir -p $OUT
  TIME=`date '+%Y-%m-%d-%H-%M-%S'`
  
  log=$OUT/benchmark-$TIME.csv
  
  echo "experiment, benchmark, time" > $log

  for run in $(seq 1 $RUNS); do
      ninja clean
      cmake $BASE
      ninja
  
      for i in `ls ${BASE}/benchmarks/shootout/*/*.r`; do
          echo $T
          T=`basename $i`;
          D="`dirname $i`/../..";
          echo -n "1 R_COMPILER_OPTIMIZE=3 R_ENABLE_JIT=3 tools/R, $T, " >> $log;
          R_COMPILER_OPTIMIZE=3 R_ENABLE_JIT=2 timeout $TIMEOUT /usr/bin/time -f'%E' -- ${SCRIPTPATH}/R -e "{setwd('$D'); source('$i'); execute()}" \
              > /dev/null 2>>$log || echo "" >>$log
          echo -n "2 R_ENABLE_JIT=0 PLAIN_R, $T, " >> $log;
          R_ENABLE_JIT=0 timeout $TIMEOUT /usr/bin/time -f'%E' -- $PLAIN_R -e "{setwd('$D'); source('$i'); execute()}" \
              > /dev/null 2>>$log || echo "" >>$log
          echo -n "3 R_COMPILER_OPTIMIZE=3 R_ENABLE_JIT=3 PLAIN_R, $T, " >> $log;
          R_COMPILER_OPTIMIZE=3 R_ENABLE_JIT=2 timeout $TIMEOUT /usr/bin/time -f'%E' -- $PLAIN_R -e "{setwd('$D'); source('$i'); execute()}" \
              > /dev/null 2>>$log || echo "" >>$log
          echo -n "4 tools/R enableJit(level=2 type='sticky'), $T, " >> $log;
          timeout $TIMEOUT /usr/bin/time -f'%E' -- ${SCRIPTPATH}/R -e "{rir.enableJit(level=2, type='sticky');setwd('$D'); source('$i'); execute()}" \
              > /dev/null 2>>$log || echo "" >>$log
          echo -n "5 tools/R enableJit(type='force'), $T, " >> $log;
          timeout $TIMEOUT /usr/bin/time -f'%E' -- ${SCRIPTPATH}/R -e "{rir.enableJit(level=2, type='force');setwd('$D'); source('$i'); execute()}" \
              > /dev/null 2>>$log || echo "" >>$log
      done 
  done
fi

R -f ${BASE}/tools/benchmark-plot.r --args $log
