loc <- 42
f <- rir.compile(function(x) {
    loc <- 1
    delayedAssign("x", rm("loc"))
    x
    loc
})

tramp <- rir.compile(function(fun) fun())

rir.markOptimize(f)
stopifnot(tramp(f) == 42)
