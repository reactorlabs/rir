require("rjit")

f <- jit.compile(function() {
	a <- c(2:5)
	a[[1]] <- 5
	a
})

stopifnot(c(5,3,4,5) == f())

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
	x[[1+1+1]] <- 5 + y - 5
	x
})
stopifnot(c(1,2,4) == f())

f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	z <- 1
	g <- function(){
		y[[2]] <<- 5 + z
	}
	g()
	y
})
stopifnot(c(1,6,3,4,5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	g <- function(){
		y[[2]] <<- 5
	}
	g()
	x
})
stopifnot(c(1:5) == f())