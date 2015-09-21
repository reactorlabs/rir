#!/bin/bash

set -e

TARGET=$1
OPT="-O0"

BUILD_DIR=`pwd`

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

SRC_DIR=`cd ${SCRIPTPATH}/.. && pwd`

if [ -z $TARGET ]; then
  echo "usage: ./${0} target_directory [ninja]"
  exit 1
fi

LLVM_TARGET=${TARGET}/llvm

if [ ! -d $TARGET ]; then
  echo "-> creating ${TARGET}"
  mkdir $TARGET
fi

CORES=`nproc || echo 8`

if [[ $2 == "ninja" ]]; then
    GEN="Ninja"
    M="ninja"
else
    GEN="Unix Makefiles"
    M="make -j${CORES}"
fi

LLVM_VERS="370"

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

LLVM_BUILD_DIR_F=llvm-build-${LLVM_VERS}
LLVM_BUILD_DIR=${LLVM_TARGET}/${LLVM_BUILD_DIR_F}
echo "-> building llvm to ${LLVM_BUILD_DIR}"
mkdir -p $LLVM_BUILD_DIR
cd $LLVM_BUILD_DIR
cmake -G "$GEN" -DLLVM_ENABLE_RTTI=1 --enable-debug-symbols --with-oprofile ${LLVM_SRC}
$M

cd $SRC_DIR

if [ ! -d gnur ]; then
    echo "-> checking out gnur" 
    git clone https://bitbucket.org/reactorl/gnur
fi

echo "-> git reset gnur to rllvm" 
cd gnur
git checkout rllvm

echo "-> configure gnur"
if [[ "$OSTYPE" == "darwin"* ]]; then
  # Mac OSX
  F77="gfortran -arch x86_64" FC="gfortran -arch x86_64" CXXFLAGS="$OPT -fno-omit-frame-pointer -gdwarf-2 -g3 -arch x86_64" CFLAGS="$OPT -fno-omit-frame-pointer -gdwarf-2 -g3 -arch x86_64" ./configure --with-blas --with-lapack --with-ICU=no --with-system-xz=no --with-system-zlib=no --with-x=no --with-readline=no --without-recommended-packages
else
    CXXFLAGS="$OPT -fno-omit-frame-pointer -gdwarf-2 -g3" CFLAGS="$OPT -fno-omit-frame-pointer -gdwarf-2 -g3" ./configure --with-blas --with-lapack --with-ICU=no --with-system-xz=no --with-system-zlib=no --with-x=no --with-readline=no --without-recommended-packages
fi
#sed -i -e "s/exit 1/\$(ECHO) \"I am feeling lucky\"/g" Makefile
echo "Revision: -99" > SVN-REVISION
rm -f non-tarball

echo "-> build gnur"
make -j${CORES}

cd $BUILD_DIR
echo "-> cmake rjit"
rm -f CMakeCache.txt
cmake -G "$GEN" -DLLVM_DIR=${LLVM_BUILD_DIR}/share/llvm/cmake $SRC_DIR
$M

echo "-> install hooks"
${SRC_DIR}/tools/install_hooks.sh

echo "-> running tests"
${SRC_DIR}/tools/tests
