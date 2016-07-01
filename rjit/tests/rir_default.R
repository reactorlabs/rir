f <- rir.compile(function(a=2) a)
stopifnot(f() == 2)

f <- rir.compile(function(a, b=a+2) b)
stopifnot(f(3) == 5)
