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

Caveat: we use submodules. If you are in the habit of blindly `git commit .` you are up for surprises. Please make sure that you do not by accident commit an updated submodule reference for external/custom-r.

### Making changes to gnur

R with RIR patches is a submodule under external/custom-r. This is how you edit:

    # Assuming you are making changes in you local RIR branch
    cd external/custom-r
    # By default submodules are checked out headless. We use a
    # branch to keep track of our changes to R, that is based on
    # one of the R version branches. If you want ot make changes
    # you have to make sure to be on that branch locally, before
    # creating commits.
    git checkout rir-patch-3-3-branch
    git pull origin rir-patch-3-3-branch
    # edit some stuff ... 
    git commit
    git push origin rir-patch-3-3-branch
    cd ../..
    # now the updated submodule needs to be commited to rir 
    git commit external/custom-r -m "bump R module version"
    git push my-remote my-feature-branch
    # Now you can create a PR with the R changes & potential RIR 
    # changes in my-feature-branch

Fetch updated R:

    git submodule update
    cd external/custom-r && make -j4 

Or use `make setup`

## Build Status

[performance](http://ginger.ele.fit.cvut.cz/~oli/r-we-fast/)

![travis](https://api.travis-ci.org/reactorlabs/rir.svg?branch=master) ([travis](https://travis-ci.org/reactorlabs/rir))
