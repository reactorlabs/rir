f <- function(...) nargs()
g <- function() f(1, 2, 3)
for (i in 1:10)
    stopifnot(g() == 3)
