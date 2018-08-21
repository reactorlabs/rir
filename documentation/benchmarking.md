# Performance
RIR was devise as an speculative compiler for boosting the speed of the R programming 
language. As such, its performance results are an essential information to trace and 
analyze. Consequently, we periodically measure RIR performance.

## Infraestructure
We track the performance of (almost) every commit made to the master branch. To share these 
results, and analyze the series of data our benchmarking infrastructure uses a 
[codespeed web server](https://github.com/tobami/codespeed). For running the benchmarks 
and generating the raw data we resort to [ReBench](https://github.com/smarr/reBench/).

## Web Server
The results can be found at: [http://rirflies.prl.r.com/](http://rirflies.prl.r.com/).
Since the domain is not public, visitors should manually bind it to our web server IP 
before the first access. This is pretty simple. Just open the file `/etc/hosts` in your 
favorite text editor (admin permissions are needed) and add the  following two lines:

    # RIR Benchmarking
    147.32.232.108 rirflies.prl.r.com

## Benchmarks
Currently we are using the Bounce and Mandelbrot benchmarks from the 
[are-we-fast-yet suite](https://github.com/smarr/are-we-fast-yet/). Below we provide some
basic information about each. For a more detailed information regarding different kind of
metrics, [visit the original paper documentation](https://github.com/smarr/are-we-fast-yet/blob/master/docs/metrics.md).

### Bounce
Bounce is a micro benchmark that simulates a box with bouncing balls. For each iteration, 
the benchmark mainly computes the x and y coordinates of a list of balls based on the current 
position plus the current velocity. As such, the benchmark mainly challenges the usage of loops, 
and basic arithmetic operations over a list of objects (balls). 
 
### Mandelbrot 
Classic is a micro benchmark implementing the classical Mandelbrot computation. For each iteration,
it mainly performs floating point operations and some basic binary arithmetic like bit shifting, and
binary logical operators. 

## Results


