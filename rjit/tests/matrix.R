require("rjit")

f <- jit.compile(function() {
	a <- matrix(c(2:5),2,2)
	a[2,2]
})
stopifnot(5 == f())