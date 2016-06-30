a <<- 3
b <<- 10
g <<- function(a, b) a + b
f <<- function(x)  g(10, 78)
fc <<- rir.compile(f)
stopifnot(fc(1) == 88)

f <- function(n)  if (n < 2) 1 else fc(n-1) + fc(n-2)
fc <- rir.compile(f)
stopifnot(fc(n=4) == 5)
