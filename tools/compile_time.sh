#!/bin/bash -e

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

. "${SCRIPTPATH}/../.local.config"
. "${SCRIPTPATH}/script_include.sh"

if [ -d "$1" ]; then
    BUILD_DIR=$1
    echo "Using non-default build dir $BUILD_DIR"
fi

run_code() {
  COMPILE_JOB=$@
  R="${R_HOME}/bin/R -q --slave"

  if test "$(uname)" = "Darwin"; then
      LIB="dyn.load('${BUILD_DIR}/librjit.dylib')"
  else
      LIB="dyn.load('${BUILD_DIR}/librjit.so')"
  fi

  TEST=$(mktemp /tmp/r-test.XXXXXX)
  echo ${LIB} > $TEST
  echo "source('${ROOT_DIR}/rjit/R/rjit.R')" >> $TEST
  echo $COMPILE_JOB >> $TEST

  $R -f $TEST
  res=$?

#  rm $TEST
}

STATUS=$(mktemp /tmp/r-test-status.XXXXXX)
COMMIT_ID=`git rev-parse HEAD`
PACKAGE='base'

COMPILE_TIME_FUNCTION="
compile_times <- list();
timestamp <- Sys.time();
benv <- getNamespace('$PACKAGE');
for (name in names(benv)) {
  x <- benv[[name]];
  if (typeof(x) == 'closure') {
    compile_times[[name]] <- list();
    start <- Sys.time();
    comp_res <- jit.compile(x);
    compile_times[[name]][['$COMMIT_ID']] <- list(timestamp, Sys.time() - start);
  }
};
filename <- paste('$PACKAGE', '_functions_', '$COMMIT_ID', '.Rds', sep = '');
saveRDS(compile_times, filename);
"

COMPILE_TIME_PACKAGE="
timestamp <- Sys.time();
benv <- getNamespace('$PACKAGE');
start <- Sys.time();
for (name in names(benv)) {
  x <- benv[[name]];
  if (typeof(x) == 'closure') {
    res <- jit.compile(x);
  }
};
compile_time <- list('$COMMIT_ID'=list(timestamp, Sys.time() - start));
filename <- paste('$PACKAGE', '_package_', '$COMMIT_ID', '.Rds', sep = '');
saveRDS(compile_time, filename);
"

run_code $COMPILE_TIME_FUNCTION
run_code $COMPILE_TIME_PACKAGE

FOLDER="teamcity/compilation"

${SCRIPTPATH}/dropbox_uploader.sh -f ${SCRIPTPATH}/.dropbox_uploader upload $PACKAGE\_functions_$COMMIT_ID\.Rds $FOLDER/
${SCRIPTPATH}/dropbox_uploader.sh -f ${SCRIPTPATH}/.dropbox_uploader upload $PACKAGE\_package\_$COMMIT_ID\.Rds $FOLDER/

