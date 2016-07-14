# RIR

# Installation

git clone https://github.com/reactorlabs/rir
cd rir
cmake .
# Fetch and build dependencies, this will build gnur from source. Note that on Mac OSX, you will need to install a fortran compiler (eg. brew install gcc). 
make setup

## Running tests

To run the basic test, just execute:

```
tools/tests
```

# GNUR tests

Currently I have the following very brittle method of running the gnur tests:

        cd gnur

        # Add a system wide Rprofile which loads rir
        cat - >> library/base/R/Rprofile <<EOF
        dyn.load('~/path/to/rir/lirir.so')
        sys.source('~/path/to/rir/rir/R/rir.R')
        rir.enableJit(1L)
        EOF

        # reset the tests folder and run the tests
        git clean -fx -e Makefile tests/ && make check
