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
