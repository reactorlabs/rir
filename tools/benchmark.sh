SCRIPTPATH=`cd $(dirname "$0") && pwd`
PLAIN_R=~/src/freshr/R-3-2-branch/bin/R
BASE=`cd $SCRIPTPATH/.. && pwd`

for i in `ls ${BASE}/benchmarks/shootout/*/*.r`; do
    T=`basename $i`;
    echo $T;
    D=`dirname $i`;
    R_JIT_ENABLE=3 /usr/bin/time -f'%E' -- ${SCRIPTPATH}/R -e "{setwd('$D'); source('$T'); execute()}" > /dev/null;
    R_JIT_ENABLE=0 /usr/bin/time -f'%E' -- ${SCRIPTPATH}/R -e "{setwd('$D'); source('$T'); execute()}" > /dev/null;
    R_JIT_ENABLE=3 /usr/bin/time -f'%E' -- $PLAIN_R -e "{setwd('$D'); source('$T'); execute()}" > /dev/null;
    R_JIT_ENABLE=2 /usr/bin/time -f'%E' -- $PLAIN_R -e "{setwd('$D'); source('$T'); execute()}" > /dev/null;
    R_JIT_ENABLE=0 /usr/bin/time -f'%E' -- $PLAIN_R -e "{setwd('$D'); source('$T'); execute()}" > /dev/null;
    /usr/bin/time -f'%E' -- ${SCRIPTPATH}/R -e "{rir.enableJit(level=2, type='sticky');setwd('$D'); source('$T'); execute()}" > /dev/null;
    /usr/bin/time -f'%E' -- ${SCRIPTPATH}/R -e "{rir.enableJit(level=3, type='sticky');setwd('$D'); source('$T'); execute()}" > /dev/null;
    /usr/bin/time -f'%E' -- ${SCRIPTPATH}/R -e "{rir.enableJit(level=3, type='force');setwd('$D'); source('$T'); execute()}" > /dev/null;
done 
