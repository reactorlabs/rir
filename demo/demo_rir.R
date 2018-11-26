require(compiler)
enableJIT(3)

j <- function() {
    res <- 0
    for (k in 1:1000000) {
        res <- res + i(a=1, b=2, c=3)
    }
    res
} 

i <- function(a, b, c) {
    id(a + b + c)
}

id <- function(x) x

# R would be quite slow  to run these
# Lets instead see how fast the bytecode compiler in R runs them

# Now lets see rjit
source("loadRjit")

rjit.internal.setFlag("recordTypes", TRUE);
rjit.internal.setFlag("recompileHot", TRUE);
rjit.internal.setFlag("useTypefeedback", TRUE);
rjit.internal.setFlag("unsafeOpt", TRUE);
rjit.internal.setFlag("staticNamedMatch", TRUE);
