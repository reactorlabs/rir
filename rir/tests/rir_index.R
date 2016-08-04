
# [[

l = c(33L, 33.2, "asdf", c(123), c(1, 1L, "asdF"), 'd', NULL, list(1,2,3), TRUE, c(FALSE, TRUE))

f <- rir.compile(function(a, b) a[[b]])
for (v in l) {
    stopifnot(f(v, 1) == v[[1]])
    stopifnot(f(v, 1L) == v[[1]])
    stopifnot(f(v, 1.1) == v[[1]])
    stopifnot(f(v, TRUE) == v[[1]])
    for (i in 1:length(v))
        stopifnot(f(v, i) == v[[i]])
}

`[[.foo` <- function(...) 33
o <- 1
class(o) <- "foo"
stopifnot(f(o, 1234) == 33)

setClass("Bar", representation(a = "numeric"))
setMethod("[[", signature(x="Bar"), function(x, i) 333)
o <- new("Bar", a=1)
stopifnot(f(o, 1234) == 333)
stopifnot(f(o, 1234) == 333)

f3 <- rir.compile(function() {
    a <- c(1,2)
    a[[i=2]]
})
stopifnot(f3() == 2)

`[[.foo` <- function(o, i) o
o <- 1
class(o) <- "foo"
f2 <- rir.compile(function(a, b) {
    (a <- a + 1)[[b]];
    stopifnot(a == 2);
})
f2(o, 1)

o <- 123
class(o) <- "Bar"
stopifnot(f(o, 1) == 123)

stopifnot(f(NULL,1) == NULL)
stopifnot(f(NULL,13) == NULL)
stopifnot(rir.compile(function() NULL[[1]]) == NULL)
stopifnot(rir.compile(function() NULL[[12]]) == NULL)
