require("rjit")

# single backets

f <- jit.compile(function() {
	a <- c(2:5)
	a[1L]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- c(2:5)
	a[2 - 1]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- c(2:5)
	a[1 + 1]
})
stopifnot(3 == f())

f <- jit.compile(function() {
	a <- c(2:5)
	a[1 * 1]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- c(2:5)
	a[4 / 2]
})
stopifnot(3 == f())

# Null index

f <- jit.compile(function() {
	a <- c("a", "b", "c", "d")
	a[NULL]
})
stopifnot(integer(0) == f())

# Non-integer vectors

f <- jit.compile(function() {
	a <- c(TRUE, FALSE, FALSE, FALSE)
	a[1]
})
stopifnot(TRUE == f())

# Logical index

f <- jit.compile(function() {
	a <- c(1:4)
	a[TRUE]
})
stopifnot(c(1:4) == f())

f <- jit.compile(function() {
	a <- c(TRUE, FALSE, FALSE, FALSE)
	a[TRUE]
})
stopifnot(c(TRUE, FALSE, FALSE, FALSE) == f())

# Non-integer index

f <- jit.compile(function() {
	a <- c(1:4)
	a[1.2]
})
stopifnot(1 == f())

f <- jit.compile(function() {
	a <- c(TRUE, FALSE, FALSE, TRUE)
	a[4.1]
})
stopifnot(TRUE == f())

# vector is an expression

f <- jit.compile(function() {
	a <- c(1:10)
	g <- function(x){x}
	g(a)[1]
})
stopifnot(1 == f())

f <- jit.compile(function() {
	a <- c(1:10)
	g <- function(x){x}
	g(a)[g(1+1)]
})
stopifnot(2 == f())

################################ double brackets ################################

f <- jit.compile(function() {
	a <- c(2:5)
	a[[1L]]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- c(2:5)
	a[[2 - 1]]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- c(2:5)
	a[[1 + 1]]
})
stopifnot(3 == f())

f <- jit.compile(function() {
	a <- c(2:5)
	a[[1 * 1]]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- c(2:5)
	a[[4 / 2]]
})
stopifnot(3 == f())


# Null index

# f <- jit.compile(function() {
# 	a <- c("a", "b", "c", "d")
# 	a[[NULL]]
# })
# Throws an error: attempt to select less than one element


# Non-integer vectors

f <- jit.compile(function() {
	a <- c(TRUE, FALSE, FALSE, FALSE)
	a[[1]]
})
stopifnot(TRUE == f())

# Logical index

f <- jit.compile(function() {
	a <- c(1:4)
	a[[TRUE]]
})
stopifnot(1 == f())

f <- jit.compile(function() {
	a <- c(TRUE, FALSE, FALSE, FALSE)
	a[[TRUE]]
})
stopifnot(TRUE == f())

# Non-integer index

f <- jit.compile(function() {
	a <- c(1:4)
	a[[1.2]]
})
stopifnot(1 == f())

f <- jit.compile(function() {
	a <- c(TRUE, FALSE, FALSE, TRUE)
	a[[4.1]]
})
stopifnot(TRUE == f())

# vector is an expression

f <- jit.compile(function() {
	a <- c(1:10)
	g <- function(x){x}
	g(a)[[1]]
})
stopifnot(1 == f())

f <- jit.compile(function() {
	a <- c(1:10)
	g <- function(x){x}
	g(a)[[g(1+1)]]
})
stopifnot(2 == f())