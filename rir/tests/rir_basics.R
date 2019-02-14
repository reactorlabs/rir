stopifnot <- function(x) x

f2 <- rir.compile(function(n) n)
f <- rir.compile(function(n) if (n) 1 else f2(FALSE))
fc <- pir.compile(f)
fc(FALSE)

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
