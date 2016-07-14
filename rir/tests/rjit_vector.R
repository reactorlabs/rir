require("rjit")

# single backets

f <- jit.compile(function() {
	2[1]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	2[2]
})
stopifnot(is.na(f()))

f <- jit.compile(function() {
	a <- c(2:5)
	a[1L]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- c(2:5)
	a[1]
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

f <- jit.compile(function() {
        a <- c(c(1,2))
        a[1]
})
stopifnot(1 == f())

f <- jit.compile(function() {
        a <- c(c(1,2))
        a[2][1]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	2[2]
})
stopifnot(is.na(f()))

f <- jit.compile(function() {
        a <- c(1,2)
        a[0]
})
stopifnot(integer(0) == f())

# Null index

f <- jit.compile(function() {
	a <- c("a", "b", "c", "d")
	a[NULL]
})
stopifnot(integer(0) == f())

f <- jit.compile(function() {
	a <- c(TRUE, FALSE, FALSE, FALSE)
	a[NULL]
})
stopifnot(integer(0) == f())

# Non-integer vectors

f <- jit.compile(function() {
	a <- c(TRUE, FALSE, FALSE, FALSE)
	a[1]
})
stopifnot(TRUE == f())

f <- jit.compile(function() {
	a <- c("a", "b", "c", "d")
	a[1]
})
stopifnot("a" == f())

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
	2[[1]]
})
stopifnot(2 == f())

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

f <- jit.compile(function() {
        a <- c(c(1,2))
        a[[1]]
})
stopifnot(1 == f())

f <- jit.compile(function() {
        a <- c(c(1,2))
        a[[2]][[1]]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	2[[1]]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	2[[2]]
})
stopifnot("subscript out of bounds" == tryCatch(f(), error = function(e) e$message))

f <- jit.compile(function() {
	a <- c(1:5)
	a[[0]]
})
stopifnot("attempt to select less than one element" == tryCatch(f(), error = function(e) e$message))

# Null index

f <- jit.compile(function() {
	a <- c("a", "b", "c", "d")
	a[[NULL]]
})
stopifnot("attempt to select less than one element" == tryCatch(f(), error = function(e) e$message))


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