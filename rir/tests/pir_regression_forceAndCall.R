# missing
f <- function() forceAndCall(1, function(zzz) missing(zzz), quote(expr=))
for (i in 1:10)
  stopifnot(f() == FALSE)

f <- function() forceAndCall(1, function(zzz) zzz, quote(expr=))
for (i in 1:10)
  stopifnot(identical(f(), quote(expr=)))

x <- as.list(function(y) 42)
f <- function() forceAndCall(1, function(zzz) missing(zzz), x[[1]])
for (i in 1:10)
  stopifnot(f() == FALSE)

f <- function() forceAndCall(1, function(zzz) zzz, x[[1]])
for (i in 1:10)
  stopifnot(identical(f(), quote(expr=)))


# specials
x <- 0L
f <- function() forceAndCall(1, `if`, TRUE, {x <<- x + 1L; 1}, {x <<- x + 1L; 2})
f(); f(); f(); f()
stopifnot(x == 4L)
x <- 0L
f <- function() forceAndCall(1, `if`, FALSE, {x <<- x + 1L; 1}, {x <<- x + 1L; 2})
f(); f(); f(); f()
stopifnot(x == 4L)
x <- 0L
f <- function() forceAndCall(2, `if`, FALSE, {x <<- x + 1L; 1}, {x <<- x + 1L; 2})
f(); f(); f(); f()
stopifnot(x == 4L)
x <- 0L
f <- function() forceAndCall(3, `if`, FALSE, {x <<- x + 1L; 1}, {x <<- x + 1L; 2})
f(); f(); f(); f()
stopifnot(x == 4L)


# builtins
x <- 0L
f <- function() forceAndCall(2, sum, {x <<- x + 1L; 1}, {x <<- x + 1L; 1}, {x <<- x + 1L; 1}, {x <<- x + 1L; 1})
f(); f(); f(); f()
stopifnot(x == 16L)


# closures
x <- 0L
foo <- function(x, y, z) {}
f <- function() forceAndCall(2, foo, {x <<- x + 1L; 1}, {x <<- x + 1L; 1}, {x <<- x + 1L; 1})
f(); f(); f(); f()
stopifnot(x == 8L)
x <- 0L
f <- function() forceAndCall(1, function(x, y, z) x + z, {x <<- x + 1L; 1}, {x <<- x + 1L; 1}, {x <<- x + 1L; 1})
f(); f(); f(); f()
stopifnot(x == 8L)
x <- 0L
foo <- function(x, y, z) x + z
f <- function() forceAndCall(1, foo, {x <<- x + 1L; 1}, {x <<- x + 1L; 1}, {x <<- x + 1L; 1})
f(); f(); f(); f()
stopifnot(x == 8L)
