#!/bin/bash

set -e

export PIR_ENABLE=off
export PIR_OSR=0

cd release
cmake ../.. >&2
ninja >&2
#RIR_PROFILING=off time bin/R -f ../recordLoop.R
#RIR_PROFILING=on  time bin/R -f ../recordLoop.R


#RIR_RECORD_ONCE=1

hyperfine -r 3 --show-output "bin/R -f ../$1"
