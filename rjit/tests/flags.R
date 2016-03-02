require("rjit")

# lets define some summation functions running in a nested loop using named args
h <- function(a, b, c, d, e, f) {
    2 * (a + b + c - d - 2) *
    3 * (a + b - c + d - 3) *
    4 * (a - b + c + d - 4)
}

g <- function(f, e, d, c, b, a) {
    res <- 0
    for (i in 1:2) {
        res <- res + h(a=b, b=c, c=d, d=e, e=f, f=a)
    }
    res
}

f <- function() {
    res <- 0
    for (i in 1:600) {
        res <- res + g(4, 5, 6, a=1, b=2, c=3)
    }
    res
}
# a helper function
recompile <- function() {
    f <<- jit.compile(f)
    g <<- jit.compile(g)
    h <<- jit.compile(h)
}

jit.setFlag("recordTypes", FALSE);
jit.setFlag("recompileHot", FALSE);
jit.setFlag("useTypefeedback", FALSE);
jit.setFlag("unsafeOpt", FALSE);
jit.setFlag("staticNamedMatch", FALSE);

recompile(); f();
jit.setFlag("recordTypes", TRUE)
recompile(); f();
jit.printTypefeedback(g)
jit.setFlag("recompileHot", TRUE)
recompile(); f();
jit.setFlag("useTypefeedback", TRUE)
recompile(); f();
jit.setFlag("unsafeOpt", TRUE)
recompile(); f();
jit.setFlag("staticNamedMatch", TRUE)
recompile(); f();
