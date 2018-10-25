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

# R would be quite slow  to run these
# Lets instead see how fast the bytecode compiler in R runs them
require(compiler)
f <- cmpfun(f)
h <- cmpfun(h)
print(system.time(f()))

# Now lets see rjit
source("loadRjit")

jit.setFlag("recordTypes", FALSE);
jit.setFlag("recompileHot", FALSE);
jit.setFlag("useTypefeedback", FALSE);
jit.setFlag("unsafeOpt", FALSE);
jit.setFlag("staticNamedMatch", FALSE);

# a helper function
recompile <- function() {
    f <<- jit.compile(f)
    g <<- jit.compile(g)
    h <<- jit.compile(h)
}

# lets run the example native
recompile()
print(system.time(f()))

# now lets start recording the types
jit.setFlag("recordTypes", TRUE)
recompile()
print(system.time(f()))

# uh this is slow, but look we got some useful info
jit.printTypefeedback(h)
jit.printTypefeedback(g)

# now let put this to use
jit.setFlag("useTypefeedback", TRUE)
jit.setFlag("recompileHot", TRUE)
recompile()
print(system.time(f()))

# well its, not too exciting yet, since we are really stupid about redundant checks
# let's disable them for a second
jit.setFlag("unsafeOpt", TRUE)
recompile()
print(system.time(f()))

# now lets do something about the named arguments:
jit.setFlag("staticNamedMatch", TRUE)
recompile()
print(system.time(f()))
