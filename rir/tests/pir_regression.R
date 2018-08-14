f <- pir.compile(rir.compile(function(a) a(b=1, 2)))
a <- function(a,b) c(a,b)
stopifnot(c(2,1) == pir.compile(rir.compile(function()f(a)))())

# forcing a promise can inspect the whole call stack
f <- pir.compile(rir.compile(function(x) sys.frame(x)))
g <- pir.compile(rir.compile(function(y) y))
h <- pir.compile(rir.compile(function() g(f(2))))
h()  # aborts if g's environment got elided
