# lets define some summation functions running in a nested loop using named args
h <- function(a, b, c, d, e, f) {
    2 * (a + b + c - d - 2) *
    3 * (a + b - c + d - 3) *
    4 * (a - b + c + d - 4)
}

g <- function(f, e, d, c, b, a) {
    res <- 0
    for (i in 1:40) {
        res <- res + h(a=b, b=c, c=d, d=e, e=f, f=a)
    }
    res
}

f <- function() {
    res <- 0
    for (i in 1:100000) {
        res <- res + g(4, 5, 6, a=1, b=2, c=3)
    }
    res
}

j <- function() {
    res <- 0
    for (i in 1:100000) {
        res <- res + j(a=1, b=2, c=3)
    }
    res
} 

i <- function(a, b, c) {
    a + b + c
}

# R would be quite slow  to run these
# Lets instead see how fast the bytecode compiler in R runs them
require(compiler)

# print out the GNUR bytecode for the function 'a'
printCompiler <- function(a) {
    disassemble(cmpfun(a))
}

# Now lets see rjit
source("loadRjit")

rjit.internal.setFlag("recordTypes", TRUE);
rjit.internal.setFlag("recompileHot", TRUE);
rjit.internal.setFlag("useTypefeedback", TRUE);
rjit.internal.setFlag("unsafeOpt", TRUE);
rjit.internal.setFlag("staticNamedMatch", TRUE);

# a helper function for Rjit
recompileRjit <- function(a) {
    rir.disableJit();
    rjit.enableJit();
    Rjit <<- rjit.compile(a)
}

# print out the RJIT bytecode for the function 'a'
printRjit <- function (a) {
    recompileRjit(a)
    rjit.print(Rjit)
}

# a helper function for Rir
recompileRir <- function (a) {
    rjit.disableJit()
    rir.enableJit()
    Rir <<- rir.compile(a)
}

# print out the RIR bytecode for the function 'a'
printRir <- function (a) {
    recompileRir(a)
    rir.print(Rir)
}
