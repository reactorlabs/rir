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
    rjit.disableJit();
    rir.disableJit();
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


# The demo instructions:

# load R

# Source the demo:

# > source("demo/demo_rir.R")
# Loading required package: compiler

# Display function 'i' - it is already defined in the demo:
# > i
# function(a, b, c) {
#     a + b + c
# }

# Display the GNUR bytecode for the function 'i' with the printCompiler function:
# > printCompiler(i)
# list(.Code, list(8L, GETVAR.OP, 1L, GETVAR.OP, 2L, ADD.OP, 3L,
#     GETVAR.OP, 4L, ADD.OP, 5L, RETURN.OP), list({
#     a + b + c
# }, a, b, a + b, c, a + b + c))

# Display the Rjit bytecode for the function 'i' with the printRjit function:
# > printRjit(i)
# define %struct.SEXPREC addrspace(1)* @rfunction(%struct.SEXPREC addrspace(1)* %consts, %struct.SEXPREC addrspace(1)* %rho, %struct.SEXPREC addrspace(1)* %closure) #0 gc "rjit" {
# start:
#   %0 = getelementptr %struct.SEXPREC, %struct.SEXPREC addrspace(1)* %consts, i64 0, i32 4, i32 1

# .....

# Display the Rir bytecode for the function 'i' with the printRir function:
# > printRir(i)
# Container length 1024.
# Function object:
#   Magic:           cafebabe (hex)
#   Size:            324
#   Origin:          unoptimized
#   Code objects:    6
#   Fun code offset: 10c (hex)
# Code object (offset 18 (hex))
#   Magic:     110000ff (hex)
#   Source:    104 (index to src pool)
#   Stack (o): 1
#   Stack (i): 0
#   Num insns: 2
#   Code size: 6 [b]
#      0    ldvar_  67 # a
#      5    ret_ 