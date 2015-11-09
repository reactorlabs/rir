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
CORES=-1
LLVM_VERS="370"
CLANG=0

function usage() {
  echo "usage: ./${0} [options]"
  echo
  echo "Options:"
  echo
  echo "-n|--ninja                Use ninja instead of make"
  echo "-f|--gnur-flags  aflag    Pass aflag as CFLAGS to gnur    Default: -O0"
  echo "-d|--deps-target path     Directory to checkout deps      Default: .."        
  echo "-l|--skip-llvm            Skip llvm"
  echo "--add-clang               additionally build clang"
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

TARGET=`cd $TARGET && pwd`

R_DIR=${TARGET}/gnur
TESTR_DIR=${TARGET}/testr

test -d ${SRC_DIR}/.git
IS_GIT_CHECKOUT=$?

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

if [ -z "$LLVM_BUILD_DIR" ]; then
    LLVM_TARGET=${TARGET}/llvm
    LLVM_BUILD_DIR_F=llvm-build-${LLVM_VERS}
    LLVM_BUILD_DIR=${LLVM_TARGET}/${LLVM_BUILD_DIR_F}
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
    cmake -G "$GEN" -DLLVM_OPTIMIZED_TABLEGEN=1 -DLLVM_ENABLE_RTTI=1 -DLLVM_TARGETS_TO_BUILD="X86;CppBackend" -DCMAKE_BUILD_TYPE=Debug --enable-debug-symbols --with-oprofile ${LLVM_SRC}

    echo "Building llvm now -- this might take quite a while.... (if it fails rerun setup with fewer threads (-j))"
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

        echo "-> pulling gnur changes"
        dirty=`git status --porcelain | wc -l`
        if [ $dirty -ne 0 ]; then
            echo -e "\e[31mWARNING:\e[0m You have unstaged changes in ${R_DIR}. Skipping git pull."
        else
            git pull
        fi
        
        echo "-> configure gnur"
        if [[ "$OSTYPE" == "darwin"* ]]; then
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
cmake -G "$GEN" -DTESTR_DIR=$TESTR_DIR -DLLVM_DIR=${LLVM_BUILD_DIR}/share/llvm/cmake -DR_HOME=$R_DIR $SRC_DIR
if [ $SKIP_BUILD -eq 0 ]; then
    #TODO: fix first build
    if [ -z `$M codegen` ]; then echo ""; fi
    $M

    echo "-> running tests"
    ${SRC_DIR}/tools/tests
fi
# save success result for CI
echo "SETUP_SUCCESS=1\n" > "${SRC_DIR}/.test_results"
