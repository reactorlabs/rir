# regression test for https://github.com/reactorlabs/rir/issues/998
# see https://github.com/r-lib/rlang/blob/6dbcae3fc9af9e75b27053b28e7ae81e0717a387/R/arg.R
# (is_missing in particular)

is_reference <- function(x, y) {
  # .Call("rlang_is_reference", x, y)  # tests pointer equality
  FALSE
}
is_missing <- function(x) missing(x) || is_reference(x, quote(expr = ))
f <- function(x) is_missing(x)

stopifnot(f())
stopifnot(f())
stopifnot(f())
stopifnot(f())



#' The missing argument is an object that triggers an error if and
#' only if it is the result of evaluating a symbol. No error is
#' produced when a function call evaluates to the missing argument
#' object. For instance, it is possible to bind the missing argument
#' to a variable with an expression like `x[[1]] <- missing_arg()`.
#' Likewise, `x[[1]]` is safe to use as argument, e.g. `list(x[[1]])`
#' even when the result is the missing object.
#' However, as soon as the missing argument is passed down between
#' functions through a bare variable, it is likely to cause a missing
#' argument error.

test <- function() {
    a <- quote(expr = )
    b <- 3
    x <- list()
    x[[1]] <- quote(expr = )
    miss <- NULL
    f <- function(x = quote(expr = )) { miss <<- missing(x); x }
    ok <- list()

    f()
    ok[[1]] <- miss

    f(quote(expr = ))
    ok[[2]] <- miss

    threw <- FALSE
    tryCatch(f(a), error = function(e) { threw <<- TRUE })
    ok[[3]] <- miss
    ok[[4]] <- threw
    ## Error in print(x) : argument "a" is missing, with no default

    f(3)
    ok[[5]] <- miss

    f(b)
    ok[[6]] <- miss

    f(x[[1]])
    ok[[7]] <- miss

    ok
}
for (i in 1:1000) test()
stopifnot(test() == c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))


f <- function(x)
  missing(x) || identical(x, quote(expr = ))

f(i)
f()
f(quote(expr = ))
for (i in 1:1000) {
  f(i)
  f()
  f(quote(expr = ))
}

stopifnot(f(1) == FALSE)
stopifnot(f() == TRUE)
stopifnot(f(quote(expr = )) == TRUE)
stopifnot((function(x) f(x))() == TRUE)
