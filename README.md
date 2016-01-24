# R + LLVM

TODO --  (from Jan)

  * Installation -- depencies should check for ninja and doxygen
                 -- how do we install? 
                 -- why are we using "setup" after the setup?
                 -- the default should be something that builds on all platforms. (For me following the instructions to the letter does not work)

  * Clean  --  how do we clean everything (including CMake)?

  * Run -- how do we run?  
        -- how do we know we are running compiled code?

  * Why?  I have only changed the README

  * When dowloading from BB, there are missing .git files / dirs
 
[![wercker status](https://app.wercker.com/status/ba5d6ebd4d90bb4d6a4a9b1f12894063/m "wercker status")](https://app.wercker.com/project/bykey/ba5d6ebd4d90bb4d6a4a9b1f12894063)

# Installation

TL;DR - complete script to get everything needed and build with ninja (-n)
```
tools/setup.sh -n
```

## Running tests

To run the basic test, just execute:

```
cmake --build . --target tests
```

## Alternative installation variants

To install dependencies to a different place
```
tools/setup.sh -d some/source/folder
```

To build off tree
```
mkdir build && cd build
../tools/setup.sh
```

More options see
```
tools/setup.sh -h
```

Subsections are meant to be possibly clear out some details about [buildin llvm](#building-llvm), [buildin GNUR and rjit](#building-modified-gnur-and-rjit) and [running tests](#running-tests). 

## Building llvm

LLVM is used for compiling R bytecode and we will build it from the source code (it should be possible to use LLVM 3.7.0 installed by package manager, but that has not been tested). 

"tools/setup.sh dir" will install and build llvm in dir/llvm 

## Building modified GNUR and rjit

Running `rjit` requres a modified version of [GNU-R](https://bitbucket.org/reactorl/gnur), so next step is to build it from source. Note that on Mac OSX, you will need to install `fortan` compiler separately from [here](https://gcc.gnu.org/wiki/GFortranBinaries#MacOS) or using [brew](http://brew.sh/) `brew install gcc` (which will also install `gfortran`).

