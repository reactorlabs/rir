require("rjit")

### Single brackets 

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
	a[1,1] <- 10
	a[1,1]
})
stopifnot(10 == f())

### Double brackets 

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
# f <- jit.compile(function() {
# 	a <- matrix(c(2:5),2,2)
# 	a[[,]]
# })
# stopifnot(matrix(c(2:5),2,2) == f())

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[[1,1]] <- 10
	a[[1,1]]
})
stopifnot(10 == f())