#!/bin/bash

set -e

TARGET=$1

SCRIPT=$(readlink -f "$0")
SCRIPTPATH=$(dirname "$SCRIPT")
SRC_DIR=`cd ${SCRIPTPATH}/.. && pwd`

if [ -z $TARGET ]; then
  echo "usage: ./${0} target_directory [ninja]"
  exit 1
fi
if [ ! -d $TARGET ]; then
  echo "-> creating ${TARGET}"
  mkdir $TARGET
fi

if [[ $2 == "ninja" ]]; then
    GEN="Ninja"
    M="ninja"
else
    GEN="Unix Makefiles"
    M="make -j8"
fi

LLVM_VERS="370"


LLVM_TARGET=${TARGET}/llvm
mkdir ${LLVM_TARGET}
cd ${LLVM_TARGET}

LLVM_SRC_F=llvm-src-${LLVM_VERS}
LLVM_SRC=${LLVM_TARGET}/${LLVM_SRC_F}
echo "-> checking out llvm ${LLVM_VERS} to ${LLVM_SRC}"

svn co http://llvm.org/svn/llvm-project/llvm/tags/RELEASE_${LLVM_VERS}/final/ llvm-src-${LLVM_VERS}

LLVM_BUILD_DIR_F=llvm-build-${LLVM_VERS}
LLVM_BUILD_DIR=${LLVM_TARGET}/${LLVM_BUILD_DIR_F}
echo "-> building llvm to ${LLVM_BUILD_DIR}"
mkdir $LLVM_BUILD_DIR
cd $LLVM_BUILD_DIR
cmake -G "$GEN" -DLLVM_ENABLE_RTTI=1 --enable-debug-symbols --with-oprofile ${LLVM_SRC}
$M

cd $SRC_DIR

if [ ! -d gnur ]; then
  echo "-> checking out gnur" 
  git clone https://bitbucket.org/reactorl/gnur
  cd gnur
  git checkout rllvm
fi

echo "-> configure gnur"
if [[ "$OSTYPE" == "darwin"* ]]; then
  # Mac OSX
  F77="gfortran -arch x86_64" FC="gfortran -arch x86_64" CXXFLAGS="-fno-omit-frame-pointer -gdwarf-2 -g3 -arch x86_64" CFLAGS="-fno-omit-frame-pointer -gdwarf-2 -g3 -arch x86_64" ./configure --with-blas --with-lapack --with-ICU=no --with-system-xz=no --with-system-zlib=no --with-x=no --with-readline=no --without-recommended-packages
else
    CXXFLAGS="-fno-omit-frame-pointer -gdwarf-2 -g3" CFLAGS="-fno-omit-frame-pointer -gdwarf-2 -g3" ./configure --with-blas --with-lapack --with-ICU=no --with-system-xz=no --with-system-zlib=no --with-x=no --with-readline=no --without-recommended-packages
fi
sed -i -e "s/exit 1/\$(ECHO) \"I am feeling lucky\"/g" Makefile
echo "Revision: -99" > SVN-REVISION
rm non-tarball

echo "-> build gnur"
make -j 8

cd ..
echo "-> cmake rjit"
cmake -G "$GEN" -DLLVM_DIR=${LLVM_BUILD_DIR}/share/llvm/cmake .
$M

echo "-> install hooks"
tools/install_hooks.sh

echo "-> running tests"
tools/tests
