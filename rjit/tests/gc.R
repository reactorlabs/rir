require("compiler")
require("rjit")

g <- function(n) {
    gc()
    n
}

add <- function(a, b) g(g(a) + g(b))
stopifnot(add(1,2) == 3)
add <- jit.compile(add)
stopifnot(add(1,2) == 3)
stopifnot(add(1,2) == 3)

fib <- function(n) if (n < 2) 1 else fib(g(n) - 1) + fib(n - 2)
stopifnot(fib(5) == 8)
fib <- jit.compile(fib)
stopifnot(fib(5) == 8)
stopifnot(fib(9) == 55)
