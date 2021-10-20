f <- function(x) {
    for (i in 1:x)
        for (j in 1:x)
            for (k in 1:x)
                break
}
rir.compile(f)
for (i in 1:100)
    f(10)
stopifnot(length(rir.functionVersions(f)) == 2)
