#!/bin/bash

set -e

export PIR_ENABLE=off
export PIR_OSR=0
#export PIR_DEBUG=PrintEarlyRir


CURRENTDIR=$(pwd)

BUILD=release
OUTERITER=15

# BENCHMARK_FOLDER=shootout
# BENCHMARK=pidigits/pidigits
# INNERITER=30


# BENCHMARK_FOLDER=shootout
# BENCHMARK=spectralnorm/spectralnorm_alt_3
# INNERITER=250

# BENCHMARK_FOLDER=areWeFast
# BENCHMARK=mandelbrot
# INNERITER=500

# BENCHMARK_FOLDER=areWeFast
# BENCHMARK=storage
# INNERITER=100



# BENCHMARK_FOLDER=shootout
# BENCHMARK=mandelbrot/mandelbrot_naive_ascii
# INNERITER=200

# BENCHMARK_FOLDER=shootout
# BENCHMARK=fasta/fasta_naive
# INNERITER=80000



# BENCHMARK_FOLDER=shootout
# BENCHMARK=fannkuch/fannkuchredux_naive
# INNERITER=9




# BENCHMARK_FOLDER=shootout
# BENCHMARK=fastaredux/fastaredux_naive
# INNERITER=80000


# BENCHMARK_FOLDER=shootout
# BENCHMARK=fasta/fasta_naive_2
# INNERITER=80000




# BENCHMARK_FOLDER=shootout
# BENCHMARK=spectralnorm/spectralnorm
# INNERITER=1200


# BENCHMARK_FOLDER=shootout
# BENCHMARK=reversecomplement/reversecomplement_2
# INNERITER=150000



# BENCHMARK_FOLDER=shootout
# BENCHMARK=knucleotide/knucleotide_brute_2
# INNERITER=2000

# BENCHMARK_FOLDER=shootout
# BENCHMARK=knucleotide/knucleotide
# INNERITER=2000

# BENCHMARK_FOLDER=shootout
# BENCHMARK=binarytrees/binarytrees_naive
# INNERITER=9




BENCHMARK_FOLDER=RealThing
BENCHMARK=volcano
INNERITER=1
OUTERITER=3


# BENCHMARK_FOLDER=shootout
# BENCHMARK=nbody/nbody_naive
# INNERITER=20000




# BENCHMARK_FOLDER=shootout
# BENCHMARK=nbody/nbody_naive_inner
# INNERITER=20000



# BENCHMARK_FOLDER=shootout
# BENCHMARK=fannkuch/fannkuchredux
# INNERITER=9


# BENCHMARK_FOLDER=simple
# BENCHMARK=scalar-while
# INNERITER=25000000



cd $BUILD
cmake ../.. >&2
ninja >&2

#RIR_RECORD_ONCE=0


cd ~/benchmarks/RBenchmarking/Benchmarks/$BENCHMARK_FOLDER
time "$CURRENTDIR/$BUILD/bin/R" -f harness.r --args $BENCHMARK $OUTERITER $INNERITER


#fij-no-type-feedback


# > /dev/null
#> /dev/null 2>&1



#hyperfine -r 2 --show-output "RIR_RECORD_ONCE=1 bin/R
