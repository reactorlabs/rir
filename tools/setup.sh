#!/bin/bash

set -e

CURRENT_DIR=`pwd`
SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi
SRC_DIR=`cd ${SCRIPTPATH}/.. && pwd`


# Defaults

GEN="Unix Makefiles"
M="make -j${CORES}"
OPT="-O0"
TARGET="${SRC_DIR}/.."
SKIP_LLVM=0
SKIP_GNUR=0
SKIP_GNUR_CONFIG=0
SKIP_BUILD=0
if [[ "$OSTYPE" == "darwin"* ]]; then
    CORES=`sysctl -n hw.ncpu || echo 8`
else
    CORES=`nproc || echo 8`
fi
LLVM_VERS="370"

function usage() {
  echo "usage: ./${0} [options]"
  echo
  echo "Options:"
  echo
  echo "-n|--ninja                Use ninja instead of make"
  echo "-f|--gnur-flags  aflag    Pass aflag as CFLAGS to gnur    Default: -O0"
  echo "-d|--deps-target path     Directory to checkout deps      Default: .."        
  echo "-l|--skip-llvm            Skip llvm"
  echo "-g|--skip-gnur            Skip gnur"
  echo "-o|--skip-gnur-conf       Skip gnur configure"
  echo "-c|--cmake-only           Only run cmake in rjit"
  echo "-j num                    Number of cores"
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
    -h|--help)
    usage
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
    *)
    echo "Flag $key unknown"
    usage
    ;;
esac
shift # past argument or value
done

TARGET=`cd $TARGET && pwd`

R_DIR=${TARGET}/gnur
TESTR_DIR=${TARGET}/testr


if [ -e ${SRC_DIR}/.local.config ]; then
    . ${SRC_DIR}/.local.config
fi

if [ -n "$BUILD_DIR" ] && [ $BUILD_DIR != $CURRENT_DIR ]; then
    echo "ERROR: Build directory changed from $BUILD_DIR to $CURRENT_DIR"
    echo "remove .local.config if this really is what you want."
    exit 1
fi
if [ -n "$R_HOME" ] && [ $R_HOME != $R_DIR ]; then
    echo "ERROR: gnur directory changed from $R_HOME to $R_DIR"
    echo "remove .local.config if this really is what you want."
    exit 1
fi
if [ -n "$LLVM_CMAKE" ] && [[ $LLVM_BUILD_DIR =~ $LLVM_CMAKE ]]; then
    echo "ERROR: llvm directory changed to $LLVM_BUILD_DIR"
    echo "remove .local.config if this really is what you want."
    exit 1
fi

if [ ! -d $TARGET ]; then
    echo "-> creating ${TARGET}"
    mkdir $TARGET
fi

if [ $SKIP_LLVM -eq 0 ]; then
    LLVM_TARGET=${TARGET}/llvm
    LLVM_BUILD_DIR_F=llvm-build-${LLVM_VERS}
    LLVM_BUILD_DIR=${LLVM_TARGET}/${LLVM_BUILD_DIR_F}

    if [ ! -d ${LLVM_TARGET} ]; then
        mkdir ${LLVM_TARGET}
    fi
    cd ${LLVM_TARGET}
    
    LLVM_SRC_F=llvm-src-${LLVM_VERS}
    LLVM_SRC=${LLVM_TARGET}/${LLVM_SRC_F}
    
    echo $LLVM_SRC
    if [ ! -d ${LLVM_SRC} ]; then
        echo "-> checking out llvm ${LLVM_VERS} to ${LLVM_SRC}"
    
        svn co http://llvm.org/svn/llvm-project/llvm/tags/RELEASE_${LLVM_VERS}/final/ llvm-src-${LLVM_VERS}
    else
        echo "-> svn revert ${LLVM_SRC}"
        cd ${LLVM_SRC}
        svn revert .
    fi
    
    echo "-> building llvm to ${LLVM_BUILD_DIR}"
    mkdir -p $LLVM_BUILD_DIR
    cd $LLVM_BUILD_DIR
    cmake -G "$GEN" -DLLVM_ENABLE_RTTI=1 -DLLVM_TARGETS_TO_BUILD="X86;CppBackend" -DCMAKE_BUILD_TYPE=Debug --enable-debug-symbols --with-oprofile ${LLVM_SRC}
    $M
fi

if [ $SKIP_GNUR -eq 0 ]; then
    if [ $SKIP_GNUR_CONFIG -eq 0 ]; then
        if [ ! -d $R_DIR ]; then
            cd $TARGET
            echo "-> checking out gnur" 
            git clone https://bitbucket.org/reactorl/gnur
        fi
        
        echo "-> git checkout gnur branch rllvm" 
        cd $R_DIR
        git checkout rllvm
        
        echo "-> configure gnur"
        if [[ "$OSTYPE" == "darwin"* ]]; then
          # Mac OSX
          F77="gfortran -arch x86_64" FC="gfortran -arch x86_64" CXXFLAGS="$OPT -fno-omit-frame-pointer -gdwarf-2 -g3 -arch x86_64" CFLAGS="$OPT -fno-omit-frame-pointer -gdwarf-2 -g3 -arch x86_64" ./configure --with-blas --with-lapack --with-ICU=no --with-system-xz=no --with-system-zlib=no --with-x=no --with-readline=no --without-recommended-packages
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

echo "-> update hooks"
${SRC_DIR}/tools/install_hooks.sh

cd $CURRENT_DIR

if [ "$GEN" == "Unix Makefiles" ] && [ -f ${BUILD_DIR}/build.ninja ]; then
    echo "ERROR: switch from ninja to make?"
    echo "add '-n' to use ninja or remove build.ninja to proceed"
    exit 1
fi

echo "-> cmake rjit"
rm -f CMakeCache.txt
cmake -G "$GEN" -DTESTR_DIR=$TESTR_DIR -DLLVM_DIR=${LLVM_BUILD_DIR}/share/llvm/cmake -DR_HOME=$R_DIR $SRC_DIR

if [ $SKIP_BUILD -eq 0 ]; then
    $M

    echo "-> running tests"
    ${SRC_DIR}/tools/tests
fi
