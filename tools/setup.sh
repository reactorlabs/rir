#!/bin/bash

set -e

CURRENT_DIR=`pwd`
SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi
SRC_DIR=`cd ${SCRIPTPATH}/.. && pwd`
. "${SCRIPTPATH}/script_include.sh"


# Defaults

GEN="Unix Makefiles"
M="make"
OPT="-O0"
TARGET="${SRC_DIR}/.."
SKIP_LLVM=0
SKIP_GNUR=0
SKIP_GNUR_CONFIG=0
SKIP_BUILD=0
SKIP_FRESHR=0
SKIP_PKG=0
SKIP_RUN=0
CORES=-1
LLVM_VERS="370"
FRESH_R_VERS="3-2"
CLANG=0
BENCH_RUN=0
BENCH_TEST=0
BENCH_TEST_NUM=1
BENCH_RUN_NUM=5
RJIT_BUILD_TYPE="Debug"
LLVM_TYPE=""
LLVM_BUILD_TYPE="Debug"
LOG_FILE_NAME="log"
USING_OSX=0
if [[ "$OSTYPE" == "darwin"* ]]; then
    USING_OSX=1
fi

function usage() {
  echo "usage: ./${0} [options]"
  echo
  echo "Options:"
  echo
  echo "-n|--ninja                Use ninja instead of make"
  echo "-f|--gnur-flags  aflag    Pass aflag as CFLAGS to gnur     Default: -O0"
  echo "-d|--deps-target path     Directory to checkout deps       Default: .."
  echo "-l|--skip-llvm            Skip llvm"
  echo "-r|--skip-freshr          Skip R-3-2"
  echo "-b|--skip-run             Skip running the benchmark"
  echo "-p|--skip-package         Skip creating the Rjit package"
  echo "--llvm-release            Build llvm in release mode"
  echo "--rjit-release            Build rjit in release mode"
  echo "--add-clang               additionally build clang"
  echo "-g|--skip-gnur            Skip gnur"
  echo "-o|--skip-gnur-conf       Skip gnur configure"
  echo "-c|--cmake-only           Only run cmake in rjit"
  echo "-j num                    Number of cores"
  echo "--build-run-bench repeat  Build llvm, gnur, rjit, and R-3-2,"
  echo "                          then run and graph the shootout benchmark"
  echo "                          for 'repeat' amount of times     Default: 10"
  echo "--test-bench              Run RJIT once over the shootout benchmark"
  echo "                          with the performance build, a log will "
  echo "                          be produced but not the graph."
  echo
  exit 1
}

while [[ $# > 0 ]]
do
key="$1"

case $key in
    -c|--cmake-only)
    SKIP_GNUR=1
    SKIP_LLVM=1
    SKIP_BUILD=1
    ;;
    -o|--skip-gnur-conf)
    SKIP_GNUR_CONFIG=1
    ;;
    -g|--skip-gnur)
    SKIP_GNUR=1
    ;;
    -l|--skip-llvm)
    SKIP_LLVM=1
    ;;
    -b|--skip-run)
    SKIP_RUN=1
    ;;
    -r|--skip-freshr)
    SKIP_FRESHR=1
    ;;
    -p|--skip-package)
    SKIP_PKG=1
    ;;
    -h|--help)
    usage
    ;;
    --add-clang)
    CLANG=1
    ;;
    -n|--ninja)
    GEN="Ninja"
    M="ninja"
    ;;
    -f|--gnur-flags)
    OPT="$2"
    shift # past argument
    ;;
    -j)
    CORES="$2"
    shift # past argument
    ;;
    -d|--deps-target)
    TARGET="$2"
    shift # past argument
    ;;
    --llvm-release)
    LLVM_TYPE="-nodebug"
    LLVM_BUILD_TYPE="Release"
    ;;
    --rjit-release)
    RJIT_BUILD_TYPE="Release"
    ;;
    --build-run-bench)
    GEN="Ninja"
    M="ninja"
    RJIT_BUILD_TYPE="Release"
    LLVM_TYPE="-nodebug"
    LLVM_BUILD_TYPE="Release"
    OPT="-O2"
    BENCH_RUN=1
    BENCH_RUN_NUM="$2"
    shift;
    ;;
    --test-bench)
    GEN="Ninja"
    M="ninja"
    RJIT_BUILD_TYPE="Release"
    LLVM_TYPE="-nodebug"
    LLVM_BUILD_TYPE="Release"
    OPT="-O2"
    BENCH_TEST=1
    ;;
    *)
    echo "Flag $key unknown"
    usage
    ;;
esac
shift # past argument or value
done

# in the case of ninja only manual job argument overrides num of cores
if [ $M == "ninja" ] && [ $CORES -ne -1 ]; then
    M="$M -j${CORES}"
fi

if [ $CORES -eq -1 ]; then
    CORES=`ncores`
fi

if [ $M != "ninja" ]; then
    M="$M -j${CORES}"
fi

# the target location for the build
TARGET=`cd $TARGET && pwd`

# the gnur, testr, and freshr directories
R_DIR=${TARGET}/gnur
TESTR_DIR=${TARGET}/testr
FRESH_R_DIR=${TARGET}/freshr

# the location of the benchmark directory
BENCH_DIR=${SRC_DIR}/benchmarks

# check the .git of the rjit directory
test -d ${SRC_DIR}/.git
IS_GIT_CHECKOUT=$?


if [ -e ${SRC_DIR}/.local.config ]; then
    . ${SRC_DIR}/.local.config
fi

if [ -z "$LLVM_BUILD_DIR" ]; then
    LLVM_TARGET=${TARGET}/llvm
    LLVM_BUILD_DIR_F=llvm-build-${LLVM_VERS}
    LLVM_BUILD_DIR=${LLVM_TARGET}/${LLVM_BUILD_DIR_F}${LLVM_TYPE}
fi

if [ -n "$BUILD_DIR" ] && [ $BUILD_DIR != $CURRENT_DIR ]; then
    echo "ERROR: Build directory changed from $BUILD_DIR to $CURRENT_DIR"
    echo "remove .local.config and CMakeCache if this really is what you want."
    exit 1
fi


BUILD_DIR=$CURRENT_DIR
if [ -n "$R_HOME" ] && [ $R_HOME != $R_DIR ]; then
    echo "ERROR: gnur directory changed from $R_HOME to $R_DIR"
    echo "remove .local.config and CMakeCache if this really is what you want."
    exit 1
fi
if [ -n "$LLVM_CMAKE" ] && [[ $LLVM_BUILD_DIR =~ $LLVM_CMAKE ]]; then
    echo "ERROR: llvm directory changed to $LLVM_BUILD_DIR"
    echo "remove .local.config and CMakeCache if this really is what you want."
    exit 1
fi

if [ ! -d $TARGET ]; then
    echo "-> creating ${TARGET}"
    mkdir $TARGET
fi

if [ "$GEN" == "Unix Makefiles" ] && [ -f ${BUILD_DIR}/build.ninja ]; then
    echo "ERROR: switch from ninja to make?"
    echo "add '-n' to use ninja or remove build.ninja to proceed"
    exit 1
fi

if [ "$GEN" == "Ninja" ]; then
    if [ -z `which ninja` ]; then
        echo "ERROR: ninja could not be found. please install"
        exit 1
    fi
fi
if [ -z `which python3` ]; then
    echo "ERROR: python3 could not be found. please install"
    exit 1
fi
if [ -z `which doxygen` ]; then
    echo "ERROR: doxygen could not be found. please install"
    exit 1
fi
if [ $IS_GIT_CHECKOUT -eq 0 ] && [ "$CI" != "true" ]; then
    if [ -z `which clang-format` ] && [ -z `which clang-format-3.5` ]; then
        echo "ERROR: you need clang-format installed. please install"
        exit 1
    fi
fi

# build LLVM if SKIP_LLVM is 0
if [ $SKIP_LLVM -eq 0 ]; then
    if [ ! -d ${LLVM_TARGET} ]; then
        mkdir ${LLVM_TARGET}
    fi

    cd ${LLVM_TARGET}

    LLVM_SRC_F=llvm-src-${LLVM_VERS}
    LLVM_SRC=${LLVM_TARGET}/${LLVM_SRC_F}

    if [ ! -d ${LLVM_SRC} ]; then
        echo "-> checking out llvm ${LLVM_VERS} to ${LLVM_SRC}"

        svn co http://llvm.org/svn/llvm-project/llvm/tags/RELEASE_${LLVM_VERS}/final/ llvm-src-${LLVM_VERS}
    else
        echo "-> svn revert ${LLVM_SRC}"
        cd ${LLVM_SRC}
        svn revert .
    fi

    if [ $CLANG -eq 1 ]; then
        if [ ! -d ${LLVM_SRC}/tools/clang ]; then
            echo "-> checking out clang"
            cd ${LLVM_SRC}/tools
            svn co http://llvm.org/svn/llvm-project/cfe/tags/RELEASE_${LLVM_VERS}/final/ clang
        fi
    fi

    echo "-> building llvm to ${LLVM_BUILD_DIR}"
    mkdir -p $LLVM_BUILD_DIR
    cd $LLVM_BUILD_DIR
    cmake -G "$GEN" -DLLVM_OPTIMIZED_TABLEGEN=1 -DLLVM_ENABLE_RTTI=1 -DLLVM_TARGETS_TO_BUILD="X86;CppBackend" -DCMAKE_BUILD_TYPE=${LLVM_BUILD_TYPE} --enable-debug-symbols --with-oprofile ${LLVM_SRC}

    echo "Building llvm now -- this might take quite a while.... (if it fails rerun setup with fewer threads (-j))"
    $M
fi

# build gnur if SKIP_GNUR is 0
if [ $SKIP_GNUR -eq 0 ]; then
    if [ $SKIP_GNUR_CONFIG -eq 0 ]; then
        if [ ! -d $R_DIR ]; then
            cd $TARGET
            echo "-> checking out gnur"
            git clone https://github.com/reactorlabs/gnur.git
        fi

        echo "-> git checkout gnur branch rllvm"
        cd $R_DIR
        git checkout rllvm

        echo "-> pulling gnur changes"
        dirty=`git status --porcelain | wc -l`
        if [ $dirty -ne 0 ]; then
            echo -e "\e[31mWARNING:\e[0m You have unstaged changes in ${R_DIR}. Skipping git pull."
        else
            git pull
        fi

        echo "-> configure gnur"
        if [ $USING_OSX -eq 1 ]; then
          # Mac OSX
          F77="gfortran -arch x86_64" FC="gfortran -arch x86_64" CXXFLAGS="$OPT -fno-omit-frame-pointer -gdwarf-2 -g3 -arch x86_64" CFLAGS="$OPT -fno-omit-frame-pointer -gdwarf-2 -g3 -arch x86_64" ./configure --with-blas --with-lapack --with-ICU=no --with-system-xz=no --with-system-zlib=no --with-x=no --without-recommended-packages
        else
            CXXFLAGS="$OPT -fno-omit-frame-pointer -gdwarf-2 -g3" CFLAGS="$OPT -fno-omit-frame-pointer -gdwarf-2 -g3" ./configure --with-blas --with-lapack --with-ICU=no --with-system-xz=no --with-system-zlib=no --with-x=no --without-recommended-packages
        fi
    fi

    cd $R_DIR

    touch doc/FAQ
    echo "Revision: -99" > SVN-REVISION
    rm -f non-tarball

    echo "-> build gnur"
    make -j${CORES}
fi

if [ ! -d $TESTR_DIR ]; then
    cd $TARGET
    echo "-> checking out testr"
    git clone https://github.com/allr/testr.git testr
else
    cd $TESTR_DIR
    git pull
fi

if [ $IS_GIT_CHECKOUT -eq 0 ]; then
    echo "-> update hooks"
    ${SRC_DIR}/tools/install_hooks.sh
fi

cd $CURRENT_DIR

echo "-> cmake rjit"
rm -f CMakeCache.txt
cmake -G "$GEN" -DTESTR_DIR=$TESTR_DIR -DLLVM_DIR=${LLVM_BUILD_DIR}/share/llvm/cmake -DR_HOME=$R_DIR -DCMAKE_BUILD_TYPE=${RJIT_BUILD_TYPE} $SRC_DIR
if [ $SKIP_BUILD -eq 0 ]; then
    #TODO: fix first build
    $M codegen
    cmake .
    $M

    echo "-> running tests"
    ${SRC_DIR}/tools/tests
fi

if [ $BENCH_TEST -eq 1 ]; then

    if [ $SKIP_PKG -eq 0 ]; then
        cd $CURRENT_DIR

        # install the rjit package
        echo "-> create the rjit packages"
        $M package_install
    fi

    TIMEN=$(date +"%H-%M-%S_%F")
    MACHINEN=$(whoami)@$(hostname)

    BENCH_NAME=benchmark-${MACHINEN}-${TIMEN}
    RESULT_DIR=${BENCH_DIR}/${BENCH_NAME}
    LOG_FILE=${RESULT_DIR}/${LOG_FILE_NAME}.txt
    SHOOT_DIR=${BENCH_DIR}/shootout/
    FRESH_R_BIN=${TARGET}/freshr/R-${FRESH_R_VERS}-branch/bin/R

    if [ ! -d ${RESULT_DIR} ]; then
        mkdir ${RESULT_DIR}
    fi

    echo "-> installing ggplot2 to ${FRESH_R_VERS_F}"
    ${FRESH_R_BIN} -e "install.packages(\"ggplot2\", repos=\"http://cran.rstudio.com/\")"

    cd ${BENCH_DIR}

    if [ $SKIP_RUN -eq 0 ]; then
    # runbench
        echo "-> start running the shootout benchmark "
        for x in ` find ${SHOOT_DIR} -name "*.r" `; do
            echo "-> running $x"
            R_LIBS_USER=${CURRENT_DIR}/packages R_ENABLE_JIT=5 ${TARGET}/gnur/bin/R -e "source(\"${SRC_DIR}/benchmarks/run.r\");runbench(\"$x\", \"${LOG_FILE}\", \"rjit\", ${BENCH_TEST_NUM})" > /dev/null
        done
        echo "-> Finished running the shootout benchmark "
    fi
fi


# freshr will be in the same directory as llvm and gnur
# Checking out a fresh version of R
if [ $BENCH_RUN -eq 1 ]; then

    if [ $SKIP_PKG -eq 0 ]; then
        cd $CURRENT_DIR

        # install the rjit package
        echo "-> create the rjit packages"
        $M package_install
    fi
    if [ $SKIP_FRESHR -eq 0 ]; then
        cd ${TARGET}

        # create the freshr directory 
        if [ ! -d ${FRESH_R_DIR} ]; then
            mkdir ${FRESH_R_DIR}
        fi

        cd ${FRESH_R_DIR}
    
        FRESH_R_VERS_F=R-${FRESH_R_VERS}-branch
        FRESH_R_SRC=${FRESH_R_DIR}/${FRESH_R_VERS_F}/

        # checkout R-3-2 from svn
        if [ ! -d ${FRESH_R_SRC} ]; then    
            echo "-> checking out ${FRESH_R_VERS_F} to ${FRESH_R_SRC}"
            svn co https://svn.r-project.org/R/branches/${FRESH_R_VERS_F}/ ${FRESH_R_SRC}
        fi
        cd ${FRESH_R_SRC}

        # download the set of recommended packages for R-3-2
        echo "-> checking out the recommended packages"
        ./tools/rsync-recommended

        # configure the make file
        echo "-> building the config file"
        ./configure

        # make the R-3-2
        echo "-> building a fresh copy of ${FRESH_R_VERS_F} to ${FRESH_R_DIR}"
        make
    fi

    SHOOT_DIR=${BENCH_DIR}/shootout/
    FRESH_R_BIN=${TARGET}/freshr/R-${FRESH_R_VERS}-branch/bin/R

    # downloading the ggplot2 package
    echo "-> installing ggplot2 to ${FRESH_R_VERS_F}"
    ${FRESH_R_BIN} -e "install.packages(\"ggplot2\", repos=\"http://cran.rstudio.com/\")"

    # Once everything is built, run the benchmark -> calculate the median -> print the graph
    # runbench -> calclog -> graphlog

    # set up the first line in the log.txt file: type, file name, run_1, ..., run_n.
    # this is required if the log.txt is loaded as a csv file, since the first line is the names of the df.


    # *********** SHOOTOUT BENCHMARK *********************
    # cd into a directory that will be scp to Jan's machine.
    # so this shouldn't be the benchmark directory

    TIMEN=$(date +"%H-%M-%S_%F")
    MACHINEN=$(whoami)@$(hostname)

    BENCH_NAME=benchmark-${MACHINEN}-${TIMEN}
    RESULT_DIR=${BENCH_DIR}/${BENCH_NAME}
    LOG_FILE=${RESULT_DIR}/${LOG_FILE_NAME}.txt

    if [ ! -d ${RESULT_DIR} ]; then
        mkdir ${RESULT_DIR}
    fi

    cd ${BENCH_DIR}

    if [ $SKIP_RUN -eq 0 ]; then
    # runbench
        echo "-> start running the shootout benchmark "
        for x in ` find ${SHOOT_DIR} -name "*.r" `; do
            echo "-> running $x"

            R_LIBS_USER=${CURRENT_DIR}/packages R_ENABLE_JIT=5 ${TARGET}/gnur/bin/R -e "source(\"${SRC_DIR}/benchmarks/run.r\");runbench(\"$x\", \"${LOG_FILE}\", \"rjit\", ${BENCH_RUN_NUM})" > /dev/null

            R_ENABLE_JIT=3 ${FRESH_R_BIN} -e "source(\"${SRC_DIR}/benchmarks/run.r\");runbench(\"$x\", \"${LOG_FILE}\", \"gnur\", ${BENCH_RUN_NUM})" > /dev/null
        done
    fi

    # there seems to be a bug when I try to graph with R_ENABLE_JIT=3
    # calclog and graphlog
    echo "-> graphing ${LOG_FILE}"
    ${FRESH_R_BIN} -e "source(\"${SRC_DIR}/benchmarks/run.r\");source(\"${SRC_DIR}/benchmarks/graphlog.r\");graphlog(calclog(\"${LOG_FILE}\"),\"${RESULT_DIR}/log-graph\")" > /dev/null

    # print config file this should include:
    # ------- the flags used to be build LLVM, GNUR, RJIT, R-3-2-Branch
    # ------- the R_ENABLE_JIT flag for both the GNUR and RJIT runs of the benchmark
    # ------- the time and date of the day
    # ------- the OS
    # ------- detail/spec of the machine
    CONFIG_FILE=${RESULT_DIR}/config.txt

    cd ${RESULT_DIR}

    # gcc --version
    gcc --version >> ${CONFIG_FILE}

    # fortran version
    gfortran --version >> ${CONFIG_FILE}

    # rjit build flags
    echo "-> RJIT build flags: -G \"$GEN\" -DTESTR_DIR=$TESTR_DIR -DLLVM_DIR=\"${LLVM_BUILD_DIR}\"/share/llvm/cmake -DR_HOME=\"$R_DIR\" -DCMAKE_BUILD_TYPE=\"${RJIT_BUILD_TYPE}\" \"$SRC_DIR\"" >> ${CONFIG_FILE}

    # gnur build flags
    echo "-> GNUR build flags: CXXFLAGS=\"$OPT\" -fno-omit-frame-pointer -gdwarf-2 -g3 CFLAGS=\"$OPT\" -fno-omit-frame-pointer -gdwarf-2 -g3 ./configure --with-blas --with-lapack --with-ICU=no --with-system-xz=no --with-system-zlib=no --with-x=no --without-recommended-packages" >> ${CONFIG_FILE}

    # llvm build flags
    echo "-> LLVM build flags: -G \"$GEN\" -DLLVM_OPTIMIZED_TABLEGEN=1 -DLLVM_ENABLE_RTTI=1 -DLLVM_TARGETS_TO_BUILD=X86;CppBackend -DCMAKE_BUILD_TYPE=\"${LLVM_BUILD_TYPE}\" --enable-debug-symbols --with-oprofile \"${LLVM_SRC}\"" >> ${CONFIG_FILE}

    # additional packages
    echo "-> Additional packages for R-3-2: ggplots2" >> ${CONFIG_FILE}

    # date
    date >> ${CONFIG_FILE}

    # cat /etc/debian_version
    cat /etc/debian_version >> ${CONFIG_FILE}

    # cat /proc/version
    cat /proc/version >> ${CONFIG_FILE}

    # cat /etc/lsb-release
    cat /etc/lsb-release >> ${CONFIG_FILE}

    # lscpu/lshw or sysctl hw
    if [ $USING_OSX -eq 1 ]; then
        lscpu >> ${CONFIG_FILE}
        lshw >> ${CONFIG_FILE}
    else
        system_profiles >> ${CONFIG_FILE}
    fi

    # tar the result directory and scp the tar into Jan's machine
    cd ${BENCH_DIR}

    echo "-> Storing the benchmark data and graph to remote site (please put up your tray table,"
    echo "store your any carry on luggage underneath the seat in front of you, and have your password ready)"

    # Jan's machine must be on the list of known hosts, a ssh-keygen for Jan's machine should have been setup
    #tar cvzf ${RESULT_DIR}.tar ${RESULT_DIR}
    #scp -p ${RESULT_DIR}.tar teamcity@reactor.ccs.neu.edu:/Users/teamcity/reactorl/benchmark_result

    # TODO Possibly delete everything created once the result are sent off?
fi



# save success result for CI
echo "SETUP_SUCCESS=1\n" > "${SRC_DIR}/.test_results"
