require(rjit)

f <- jit.compile(function() {
	a <- c(2:5)
	a[1] <- 5
	a
})
stopifnot(c(5,3,4,5) == f())

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

stopifnot(c(1,2,14) == f())

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