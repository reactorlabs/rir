# RIR

# Installation

    git clone https://github.com/reactorlabs/rir
    cd rir
    cmake .
    # Fetch and build dependencies. This will build gnur from source, which
    # takes a while. Note that on Mac OSX, you will need to install a fortran
    # compiler (eg. brew install gcc). 
    make setup
    make

## Running tests

To run the basic test, just execute

    tools/tests

To run tests from gnur with rir enabled as a jit:

    tools/gnur-make check-devel

## Playing with RIR

To run R with RIR use

    tools/R

This loads a normal R environment with RIR replacing the R bytecode compiler
and interpreter. If you want to automatically compile functions when they
are executed use

    R_ENABLE_JIT=2 tools/R

Functions compiled to RIR can be inspected using `rir.disassemble`.

## Hacking

To make changes to this repository please open a pull request. Ask somebody to
review your changes and make sure travis is green.

## Build Status

[performance](http://ginger.ele.fit.cvut.cz/~oli/r-we-fast/)

![travis](https://api.travis-ci.org/reactorlabs/rir.svg?branch=master) ([travis](https://travis-ci.org/reactorlabs/rir))
