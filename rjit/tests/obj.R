require("rjit")
require(compiler)

t0 <- function(theCompiler) {
    foo.bar <- function(a) 42
    foo <- function(a) UseMethod("foo", a)
    a <- 1
    attr(a, "class") <- "bar"
    res <- theCompiler(function() foo(a))()
    stopifnot(res == 42)
}

t1 <- function(theCompiler) {
    `[.bla1` <- function(a,b) 4   # do nothing to b
    a <- 1
    attr(a, "class") <- "bla1"
    res <- theCompiler(function() a[13])()
    stopifnot(res == 4)
}
    
    
t2 <- function(theCompiler) {
    `[.bla` <- function(a,b) 4   # do nothing to b
    a <- 1
    attr(a, "class") <- "bla"
    test <- 1
    delayedAssign("b", test <- 2)
    res <- theCompiler(function() a[b])()
    stopifnot(res == 4)
    stopifnot(test == 1)
}
    
t3 <- function(theCompiler) {
    `+.bar` <- function(a,b) 4   # do nothing to b
    a <- 1
    attr(a, "class") <- "bar"
    res <- theCompiler(function() a+13)()
    stopifnot(res == 4)
}
    
    
t4 <- function(theCompiler) {
    `+.foo` <- function(a,b) 4   # do nothing to b
    a <- 1
    attr(a, "class") <- "foo"
    test <- 1
    delayedAssign("b", test <- 2)
    res <- theCompiler(function() a+b)()
    stopifnot(res == 4)
    stopifnot(test == 2)
}

# Make sure the test make sense
t0(cmpfun)
t1(cmpfun)
t2(cmpfun)
t3(cmpfun)
t4(cmpfun)

# TODO Fix those
t0(jit.compile)
t1(jit.compile)
# t2(jit.compile)
# t3(jit.compile)
# t4(jit.compile)
