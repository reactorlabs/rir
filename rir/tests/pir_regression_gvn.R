# TODO(mhyee): this will be subsumed by the loop peeling tests

f <- pir.compile(rir.compile(function() {
    sum <- 0
    for (i in 1:10) {
        if (i == 5) next
        sum <- sum + i
    }
    sum
}))
stopifnot(f() ==  50)
