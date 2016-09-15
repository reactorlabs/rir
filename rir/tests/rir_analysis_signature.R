f <- rir.compile(function(a) a)
x <- rir.analysis.signature(f)
stopifnot(x["a"] == "yes")

f <- rir.compile(function(a) if (b) a )
x <- rir.analysis.signature(f)
stopifnot(x["a"] == "maybe")

f <- rir.compile(function(a) if (b) d )
x <- rir.analysis.signature(f)
stopifnot(x["a"] == "no")

f <- rir.compile(function(a, b) if (b) a else c )
x <- rir.analysis.signature(f)
stopifnot(x["a"] == "maybe")
stopifnot(x["b"] == "yes")

f <- rir.compile(function(a, b) if (b) a else a )
x <- rir.analysis.signature(f)
stopifnot(x["a"] == "yes")
stopifnot(x["b"] == "yes")

f <- rir.compile(function(a, b) { a <- 56; a ; b })
x <- rir.analysis.signature(f)
stopifnot(x["a"] == "no")
stopifnot(x["b"] == "yes")

f <- rir.compile(function(a, b) { if (g) a <- 56; a ; b })
x <- rir.analysis.signature(f)
stopifnot(x["a"] == "maybe")
stopifnot(x["b"] == "yes")
