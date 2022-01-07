x <- structure(as.list(7:8), class = "foo")
`[[.foo` <- function(x, i) if (i == 1) i else 100
f <- function(x) x[[2]]

for (i in 1:100)
    stopifnot(f(x) == 100)
