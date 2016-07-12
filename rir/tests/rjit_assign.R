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

################################### SINGLE BRACKETS ###################################

f <- jit.compile(function(){
	x <- c(1:5)
	x[1] <<- 5L
	x
})
stopifnot("object 'x' not found" == tryCatch(f(), error = function(e) e$message))


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
		x[2] <<- 5
	}
	g()
	y
})
stopifnot(c(1:5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	h <- function(){
		g <- function(){
			y[2] <<- 5
		}
		g()
	}
	h()
	y
})
stopifnot(c(1,5,3,4,5) == f())

f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	h <- function(){
		g <- function(){
			x[2] <<- 5
		}
		g()
	}
	h()
	y
})
stopifnot(c(1:5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	h <- function(){
		g <- function(){
			y[2] <<- 5
		}
		g()
	}
	h()
	x
})
stopifnot(c(1:5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	g <- function(){
		x[2] <<- 5L
	}
	g()
	x
})
stopifnot(c(1,5,3,4,5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	g <- function(){
		y[2] <<- 5L
	}
	g()
	x
})
stopifnot(c(1:5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	h <- function(){
		g <- function(){
			x[2] <<- 5
		}
		g()
	}
	h()
	x
})
stopifnot(c(1,5,3,4,5) == f())

f <- jit.compile(function(){
	x <- c(1:5)
	x[TRUE] <<- 5
	x
})
stopifnot("object 'x' not found" == tryCatch(f(), error = function(e) e$message))

f <- jit.compile(function(){
	x <- c(1:5)
	x[NULL] <<- 5
	x
})
stopifnot("object 'x' not found" == tryCatch(f(), error = function(e) e$message))

f <- jit.compile(function(){
	x <- c(1:5)
	x[] <<- 5
	x
})
stopifnot("object 'x' not found" == tryCatch(f(), error = function(e) e$message))

################################### DOUBLE BRACKETS ###################################

f <- jit.compile(function(){
	x <- c(1:5)
	x[[1]] <<- 5
	x
})
stopifnot("object 'x' not found" == tryCatch(f(), error = function(e) e$message))


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	g <- function(){
		y[[2]] <<- 5
	}
	g()
	y
})
stopifnot(c(1,5,3,4,5) == f())

f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	g <- function(){
		x[[2]] <<- 5
	}
	g()
	y
})
stopifnot(c(1:5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	h <- function(){
		g <- function(){
			y[[2]] <<- 5
		}
		g()
	}
	h()
	y
})
stopifnot(c(1,5,3,4,5) == f())

f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	h <- function(){
		g <- function(){
			x[[2]] <<- 5
		}
		g()
	}
	h()
	y
})
stopifnot(c(1:5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	h <- function(){
		g <- function(){
			y[[2]] <<- 5
		}
		g()
	}
	h()
	x
})
stopifnot(c(1:5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	g <- function(){
		x[[2]] <<- 5L
	}
	g()
	x
})
stopifnot(c(1,5,3,4,5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	g <- function(){
		y[[2]] <<- 5L
	}
	g()
	x
})
stopifnot(c(1:5) == f())


f <- jit.compile(function(){
	x <- c(1:5)
	y <- x
	h <- function(){
		g <- function(){
			x[[2]] <<- 5
		}
		g()
	}
	h()
	x
})
stopifnot(c(1,5,3,4,5) == f())

f <- jit.compile(function(){
	x <- c(1:5)
	x[[TRUE]] <<- 5
	x
})
stopifnot("object 'x' not found" == tryCatch(f(), error = function(e) e$message))

f <- jit.compile(function(){
	x <- c(1:5)
	x[[NULL]] <<- 5
	x
})
stopifnot("object 'x' not found" == tryCatch(f(), error = function(e) e$message))

f <- jit.compile(function(){
	x <- c(1:5)
	x[[]] <<- 5
	x
})
stopifnot("object 'x' not found" == tryCatch(f(), error = function(e) e$message))


f <- jit.compile(function(){
	x <- c(1:5)
	g <- function(){
		x[[]] <<- 5
	}
	g()
	x
})
stopifnot("[[ ]] with missing subscript" == tryCatch(f(), error = function(e) e$message))