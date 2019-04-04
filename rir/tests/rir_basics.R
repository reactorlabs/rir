a <<- 3
b <<- 10
g <<- function(a, b) a + b
f <<- function(x)  g(10, 78)
fc <<- rir.compile(f)
stopifnot(fc(1) == 88)

f <- function(n)  if (n < 2) 1 else fc(n-1) + fc(n-2)
fc <- rir.compile(f)
stopifnot(fc(n=4) == 5)

f <- rir.compile(function(a, b, c, x, d=44) c(d, c, b, a))
print(f(1,2,3))
stopifnot(f(1,2,3) == c(44,3,2,1))
stopifnot(f(1,2,3,4) == c(44,3,2,1))
stopifnot(f(d=1,2,3,4) == c(1,4,3,2))

f <- rir.compile(function(x) +x)
stopifnot(f(10L) == 10L)
stopifnot(f(-3.14) == -3.14)
stopifnot(f(-1:3) == c(-1, 0, 1, 2, 3))
stopifnot(f(0) == 0)
stopifnot(is.na(f(NA)))

f <- rir.compile(function(x) -x)
stopifnot(f(10L) == -10L)
stopifnot(f(-3.14) == 3.14)
stopifnot(f(-1:3) == c(1, 0, -1, -2, -3))
stopifnot(f(0) == 0)
stopifnot(is.na(f(NA)))
