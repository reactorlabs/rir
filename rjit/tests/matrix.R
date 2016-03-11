require("rjit")

############ Single brackets ############

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[2,2]
})
stopifnot(5 == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[1,1]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[1+1,1+1]
})
stopifnot(5 == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[,]
})
stopifnot(matrix(c(2:5),2,2) == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	b <- a[1,1]
	b
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[TRUE,TRUE]
})
stopifnot(matrix(c(2:5),2,2) == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	g <- function(x){x}
	g(a)[g(1),g(1)]
})
stopifnot(2 == f())

############ Double brackets ############

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[[2,2]]
})
stopifnot(5 == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[[1,1]]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[[1+1,1+1]]
})
stopifnot(5 == f())

# Gives an error
f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[[,]]
})
stopifnot("invalid subscript type 'symbol'" == tryCatch(f(), error = function(e) e$message))

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	b <- a[[1,1]]
	b
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[[TRUE,TRUE]]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	g <- function(x){x}
	g(a)[[g(1),g(1)]]
})
stopifnot(2 == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	g <- function(){a[[1,1]] <-10}
	g()
	a
})
stopifnot(matrix(c(2:5),2,2) == f())


######################### SUPER ASSIGNMENT #########################
