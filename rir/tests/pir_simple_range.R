f <- rir.compile(function(depth) {
    if (depth == 1) {
      1
    } else {
        x <- 0
        for (i in 1:4) {
            x <- x + f(depth - 1)
        }
        x
    }
})
stopifnot(f(4) == 64)

f <- rir.compile(function() {
  x <- 1
  for (i in 1L:x) NULL
})
f()
f()
f()

f <- rir.compile(function() {
    a <- 0
    for (i in 1:1) {
      a <- i
    }
    a
})
print(f())
stopifnot(f() == 1)
pir.compile(f)
print(f())
stopifnot(f() == 1)

f <- rir.compile(function(fc) {
  nfc <- length(fc)
  for(j in 2:nfc) {
  }
  nfc
})

f(c(1, 2))
f(c(1, 2))
f(c(1, 2))
stopifnot(f(c(1, 2, 3, 4)) == 4)

# Factor tests
# Test when lhs and rhs are
# - Scalar (regular)
# - Vector (warning)
# - Factor (slowcase if both are factors)
# - String (error)

f <- function(lhs, rhs) {
  x <- list(10)
  i <- 1
  for (elem in lhs:rhs) {
    x[[i]] <- elem
    i <- i + 1
  }
  x
}
g <- function(lhs, rhs) tryCatch(
  f(lhs, rhs),
  error=function(error) paste("error:", conditionMessage(error)),
  warning=function(warning) tryCatch(
    {suppressWarnings(f(lhs, rhs)); paste("warning:", conditionMessage(warning))},
    error=function(error) paste("error:", conditionMessage(error), "+ warning:", conditionMessage(warning))
  )
)
test <- function(lhs, rhs, expected) {
  print(g(lhs, rhs))
  stopifnot(toString(g(lhs, rhs)) == toString(expected))
  stopifnot(toString(g(lhs, rhs)) == toString(expected))
  stopifnot(toString(g(lhs, rhs)) == toString(expected))
}

rhs <- 7
lhs <- 5L
test(lhs, rhs, list(5, 6, 7))

lhs <- c(1, 2, 3)
test(lhs, rhs, "warning: numerical expression has 3 elements: only the first used")

lhs <- factor(lhs)
test(lhs, rhs, "warning: numerical expression has 3 elements: only the first used")

lhs <- "foobar"
test(lhs, rhs, "error: NA/NaN argument + warning: NAs introduced by coercion")

rhs <- c(7, 8, 9)
lhs <- 5L
test(lhs, rhs, "warning: numerical expression has 3 elements: only the first used")

lhs <- c(1, 2, 3)
test(lhs, rhs, "warning: numerical expression has 3 elements: only the first used")

lhs <- factor(lhs)
test(lhs, rhs, "warning: numerical expression has 3 elements: only the first used")

lhs <- "foo-bar"
test(lhs, rhs, "error: NA/NaN argument + warning: numerical expression has 3 elements: only the first used")

rhs <- factor(rhs)
lhs <- 5L
test(lhs, rhs, "warning: numerical expression has 3 elements: only the first used")

lhs <- c(1, 2, 3)
test(lhs, rhs, "warning: numerical expression has 3 elements: only the first used")

lhs <- factor(lhs)
test(lhs, rhs, list("1:7", "2:8", "3:9"))

lhs <- "foo-bar"
test(lhs, rhs, "error: NA/NaN argument + warning: numerical expression has 3 elements: only the first used")

rhs <- "foo-bar"
lhs <- 5L
test(lhs, rhs, "error: NA/NaN argument + warning: NAs introduced by coercion")

lhs <- c(1, 2, 3)
test(lhs, rhs, "error: NA/NaN argument + warning: numerical expression has 3 elements: only the first used")

lhs <- factor(lhs)
test(lhs, rhs, "error: NA/NaN argument + warning: numerical expression has 3 elements: only the first used")

lhs <- "foo-bar"
test(lhs, rhs, "error: NA/NaN argument + warning: NAs introduced by coercion")

## Test with integer min and max
rhs <- 2147483647L
lhs <- 2147483646L
test(lhs, rhs, list(2147483646L, 2147483647L))

rhs <- -2147483646L
lhs <- -2147483645L
test(lhs, rhs, list(-2147483645L, -2147483646L))

f <- function(lhs, rhs) {
  x <- list(10)
  i <- 1
  for (elem in lhs:rhs) {
    x[[i]] <- elem
    i <- i + 1
  }
  x
}

# Warmup
f(1L, 2L)
f(-1L, -2L)
f(0L, 0L)

# Try with deopt and recompile
f(2147483646L, 2147483647L)
f(2147483646L, 2147483647L)
f(-2147483645L, -2147483646L)
f(-2147483645L, -2147483646L)
f(2147483646L, 2147483647L)
f(2147483646L, 2147483647L)
f(-2147483645L, -2147483646L)
f(-2147483645L, -2147483646L)

f(2147483646, 2147483647)
f(2147483646, 2147483647)
f(2147483646, 2147483647)
f(2147483646, 2147483647)
f(1, 2)

scalarFor <- function(n) {
  for (i in 1:n) {
    
  }
  FALSE
}

stopifnot(tryCatch(scalarFor(10000000000L), error=function(err) TRUE))