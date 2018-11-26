f <- rir.compile(function(...) ..1)
stopifnot(f(1,2,3) == 1)

f <- rir.compile(function(a,b,c,d) c(a,b,c,d))
g <- rir.compile(function(a, ..., b) f(..., a, b))
h <- rir.compile(function() g(b=4, 1,2,3))
stopifnot(h() == c(2,3,1,4))
