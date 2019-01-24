# Performance
RIR was devise as an speculative compiler for boosting the speed of the R programming 
language. As such, its performance results are an essential information to trace and 
analyze. Consequently, we periodically measure RIR performance. The results can be 
found at: [http://rflies.rir.o1o.ch/](http://rflies.rir.o1o.ch/).

## Infraestructure
We track the performance of (almost) every commit made to the master branch. To share these 
results, and analyze the series of data our benchmarking infrastructure uses a 
[codespeed web server](https://github.com/tobami/codespeed). For running the benchmarks 
and generating the raw data we resort to [ReBench](https://github.com/smarr/reBench/).

## Run Locally
To run the benchmarks locally you first need to download them:
    ./tools/downloadBenchs.sh

A new folder named `benchmarks` will be added to your repo containing all the 
benchmarks and the rebench configuration file for running them.

Afterwards, you only need to run them. Note that if you do not have ReBench installed,
you will have to install. Our running script does that:
    ./tools/runBenchs.sh 
    or if you do not have rebench
    ./tools/runBenchs.sh --installRebench

When the runs finished, a file with the results will appear inside the `benchmarks` folder.

## Benchmarks
Currently we are using the Bounce, Mandelbrot and Storage benchmarks from the 
[are-we-fast-yet suite](https://github.com/smarr/are-we-fast-yet/). Below we provide some
basic information about each. For a more detailed information regarding different kind of
metrics [visit the original paper documentation](https://github.com/smarr/are-we-fast-yet/blob/master/docs/metrics.md).

### Bounce
Bounce is a micro benchmark that simulates a box with bouncing balls. For each iteration, 
the benchmark mainly computes the *x* and *y* coordinates of a list of balls based on the current 
position plus the current velocity. As such, the benchmark mainly challenges the usage of loops, 
and basic arithmetic operations over a list of objects (balls). 
 
### Mandelbrot 
Classic is a micro benchmark implementing the classical Mandelbrot computation. For each iteration,
it mainly performs floating point operations and some basic binary arithmetic like bit shifting, and
binary logical operators. 

### Storage

## Results
TODO

