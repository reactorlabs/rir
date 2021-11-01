x <- 1:4
dim(x) <- c(2, 2)
f <- function(x) is.vector(x)
for (i in 1:10)
  stopifnot(f(x) == FALSE)
