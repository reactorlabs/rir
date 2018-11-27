#!/bin/bash -e

# compile_time.sh runs the tests in rjit.
# run_time.sh runs the shootout benchmark, calculate the log, and graph the calculations.
# Assume the following has been built: llvm, rjit, gnur, R-3-2, rjit package, R-3-2 packages

CURRENT_DIR=`pwd`
SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi
SRC_DIR=`cd ${SCRIPTPATH}/.. && pwd`
. "${SCRIPTPATH}/script_include.sh"
. "${SCRIPTPATH}/../.local.config"

TARGET="${SRC_DIR}/.."
BENCH_DIR=${SRC_DIR}/benchmarks
LOG_FILE_NAME="log"
FRESH_R_DIR="${TARGET}/freshr"
FRESH_R_VERS="3-2"
BENCH_RUN_NUMBER=5
SHOOT_DIR=${BENCH_DIR}/shootout/

TIMEN=$(date +"%H-%M-%S_%F")
MACHINEN=$(whoami)@$(hostname)

RESULT_DIR=${BENCH_DIR}/benchmark-${MACHINEN}-${TIMEN}
LOG_FILE=${RESULT_DIR}/${LOG_FILE_NAME}.txt

if [ ! -d ${RESULT_DIR} ]; then
   mkdir ${RESULT_DIR}
fi

if [ ! -d "$FRESH_R_DIR" ]; then
  build_freshr $FRESH_R_DIR $FRESH_R_VERSION "-O2"
fi

FRESH_R_BIN=${FRESH_R_DIR}/R-${FRESH_R_VERS}-branch/bin/R

echo "-> installing ggplot2 to ${FRESH_R_VERS_F}"
${FRESH_R_BIN} -e "install.packages(\"ggplot2\", repos=\"http://cran.rstudio.com/\")"

cd ${BENCH_DIR}

echo "-> start running the shootout benchmark "
for x in ` find ${SHOOT_DIR} -name "*.r" `; do
    echo "-> running $x"

    R_LIBS_USER=${CURRENT_DIR}/packages R_ENABLE_JIT=5 ${TARGET}/gnur/bin/R -e "source(\"${SRC_DIR}/benchmarks/run.r\");runbench(\"$x\", \"${LOG_FILE}\", \"rjit\", ${BENCH_RUN_NUMBER})" > /dev/null

    R_ENABLE_JIT=3 ${FRESH_R_BIN} -e "source(\"${SRC_DIR}/benchmarks/run.r\");runbench(\"$x\", \"${LOG_FILE}\", \"gnur\", ${BENCH_RUN_NUMBER})" > /dev/null
done


# calclog and graphlog
echo "-> calcuting the log and generating the csv file for ${LOG_FILE}"
${FRESH_R_BIN} -e "source(\"${SRC_DIR}/benchmarks/run.r\");source(\"${SRC_DIR}/benchmarks/graphlog.r\");graphlog(calclog(\"${LOG_FILE}\"),\"${RESULT_DIR}/log-graph\")" > /dev/null

FOLDER="teamcity/execution"

COMMIT_ID=`git rev-parse HEAD`

if [ ! -z "$TEAMCITY_VERSION" ]; then
  ${SCRIPTPATH}/dropbox_uploader.sh -f ${SCRIPTPATH}/.dropbox_uploader upload ${LOG_FILE} $FOLDER/${TIMEN}_${COMMIT_ID}.txt
fi
