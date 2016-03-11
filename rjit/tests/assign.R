require("rjit")

######################### SINGLE BRACKET ASSIGNMENT #########################

f <- jit.compile(function() {
	a <- c(2:5)
	a[1] <- 5
	a
})
stopifnot(c(5,3,4,5) == f())

f <- jit.compile(function() {
        a <- 2
        g <- function(){a[1] <-3}
        a
})
f()
stopifnot(2 == f())

# We are not handling this fast case at the moment.
f <- jit.compile(function() {
        a <- c(c(1,2))
        a[1][1] <- 5
        a
})
stopifnot(c(5,2) == f())

f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	x[2] <- 5
	y
})
stopifnot(c(1:5) == f())

f <- jit.compile(function(){
	x <- c(1:3)
	x[1+1+1] <- 5
	x
})

stopifnot(c(1,2,5) == f())

f <- jit.compile(function(){
	x <- c(1:3)
	y <- 4
	x[1+1+1] <- 5 + y + 5
	x
})

f <- jit.compile(function() {
	x <- c(1:3)
	t <- x[1]; x[1] <- x[2]; x[2] <- t
	x
})
stopifnot(c(2,1,3) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	for(i in 1:4){
		x[i] <- x[i+1]
	}
	x
})

stopifnot(c(2,3,4,5,5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	for(i in 1:4){
		x[i] <- x[i+1]
	}
	y
})
stopifnot(c(1:5) == f())


f <- jit.compile(function() {
	a <- c(1,2)
	g <- function(){a <- c(10,10); a[1] <-2}
	g()
	a
})
stopifnot(c(1,2) == f())


f <- jit.compile(function() {
	a <- c(1,2)
	g <- function(){a[1] <-2}
	g()
	a
})
stopifnot(c(1,2) == f())


f <- jit.compile(function() {
	a <- c(c(1,2))
	g <- function(){a[1] <-2}
	g()
	a
})
stopifnot(c(1,2) == f())


######################### SINGLE BRACKET MATRIX ASSIGNMENT #########################

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	g <- function(){a[1,1] <-10}
	g()
	a
})
stopifnot(matrix(c(2:5),2,2) == f())

######################### DOUBLE BRACKET ASSIGNMENT #########################

f <- jit.compile(function() {
	a <- c(2:5)
	a[[1]] <- 5
	a
})
stopifnot(c(5,3,4,5) == f())

f <- jit.compile(function() {
        a <- 2
        g <- function(){a[[1]] <-3}
        a
})
f()
stopifnot(2 == f())

# We are not handling this fast case at the moment.
f <- jit.compile(function() {
        a <- c(c(1,2))
        a[[1]][[1]] <- 5
        a
})
stopifnot(c(5,2) == f())

f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	x[[2]] <- 5
	y
})
stopifnot(c(1:5) == f())

f <- jit.compile(function(){
	x <- c(1:3)
	x[[1+1+1]] <- 5
	x
})

stopifnot(c(1,2,5) == f())

f <- jit.compile(function(){
	x <- c(1:3)
	y <- 4
	x[[1+1+1]] <- 5 + y + 5
	x
})

f <- jit.compile(function() {
	x <- c(1:3)
	t <- x[[1]]; x[[1]] <- x[[2]]; x[[2]] <- t
	x
})
stopifnot(c(2,1,3) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	for(i in 1:4){
		x[[i]] <- x[[i+1]]
	}
	x
})

stopifnot(c(2,3,4,5,5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	for(i in 1:4){
		x[[i]] <- x[[i+1]]
	}
	y
})
stopifnot(c(1:5) == f())


f <- jit.compile(function() {
	a <- c(1,2)
	g <- function(){a <- c(10,10); a[[1]] <-2}
	g()
	a
})
stopifnot(c(1,2) == f())


f <- jit.compile(function() {
	a <- c(1,2)
	g <- function(){a[[1]] <-2}
	g()
	a
})
stopifnot(c(1,2) == f())


f <- jit.compile(function() {
	a <- c(c(1,2))
	g <- function(){a[[1]] <-2}
	g()
	a
})
stopifnot(c(1,2) == f())


######################### SUPER ASSIGNMENT #########################

f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	g <- function(){
		y[2] <<- 5
	}
	g()
	y
})

stopifnot(c(1,5,3,4,5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	g <- function(){
		y[2] <<- 5
	}
	g()
	x
})
stopifnot(c(1:5) == f())