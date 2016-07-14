# RIR

# Installation

    git clone https://github.com/reactorlabs/rir
    cd rir
    cmake .
    # Fetch and build dependencies. This will build gnur from source, which takes a while.
    # Note that on Mac OSX, you will need to install a fortran compiler (eg. brew install gcc). 
    make setup
    make

## Running tests

To run the basic test, just execute:

    tools/tests

To run tests from gnur with rir enabled as a jit:

    tools/gnur-make check

