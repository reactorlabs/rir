# R + LLVM

# Installation

TL;DR - complete script to get everything needed is available at [Etherpad](https://etherpad.mozilla.org/aXN4g6hiTW). Subsections are meant to be possibly clear out some details about [buildin llvm](#building-llvm), [buildin GNUR and rjit](#building-modified-gnur-and-rjit) and [running tests](#running-tests). 

## Building llvm

LLVM is used for compiling R bytecode and we will build it from the source code (it sould be possible to use LLVM installed by package manager, but that has not been tested). 

```bash
svn co https://llvm.org/svn/llvm-project/llvm/tags/RELEASE_370/final/ llvm-src
mkdir llvm-build
cd llvm-build
cmake -G 'Unix Makefiles' -DLLVM_ENABLE_RTTI=1 --enable-debug-symbols --with-oprofile ../llvm-src
make -j 8
cd ../
```

## Building modified GNUR and rjit

Running `rjit` requres a modified version of [GNU-R](https://bitbucket.org/reactorl/gnur), so next step is to build it from source. Note that on Mac OSX, you will need to install `fortan` compiler separately from [here](https://gcc.gnu.org/wiki/GFortranBinaries#MacOS) or using [brew](http://brew.sh/) `brew install gcc` (which will also install `gfortran`).

```bash
git clone https://bitbucket.org/reactorl/rjit.git
cd rjit
git clone https://bitbucket.org/reactorl/gnur
cd gnur
git checkout rllvm
# configure and build
if [[ "$OSTYPE" == "darwin"* ]]; then
# Mac OSX
        F77="gfortran -arch x86_64" FC="gfortran -arch x86_64" CPPFLAGS="-O0 -gdwarf-2 -g3 -arch x86_64" CFLAGS="-O0 -gdwarf-2 -g3 -arch x86_64" ./configure --with-blas --with-lapack --with-ICU=no --with-system-xz=no --with-system-zlib=no --with-x=no --with-readline=no --without-recommended-packages
else
        CPPFLAGS="-O0 -gdwarf-2 -g3" CFLAGS="-O0 -gdwarf-2 -g3" ./configure --with-blas --with-lapack --with-ICU=no --with-system-xz=no --with-system-zlib=no --with-x=no --with-readline=no --without-recommended-packages
fi
sed -i -e "s/exit 1/\$(ECHO) \"I am feeling lucky\"/g" Makefile
make -j 8

cd ..
# build rjit
cmake -DLLVM_DIR=../llvm-build/share/llvm/cmake .
make
```

## Running tests

To run the basic test, just execute:

```
gnur/bin/R -f tests.R
```