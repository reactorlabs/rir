dyn.load("librjit.so")
source("rjit/R/rjit.R")
library(compiler)

jit.initialize()

f <- jit(cmpfun(function(a, b) a+b))
f(1,2)


a <- jit(cmpfun(function(a) f(a, 2)))
a(1)

source("rjit/tests/simple.R")
