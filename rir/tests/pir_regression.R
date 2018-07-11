f <- pir.compile(rir.compile(function(a) a(b=1, 2)))
a <- function(a,b) c(a,b)
stopifnot(c(2,1) == pir.compile(rir.compile(function()f(a)))())
