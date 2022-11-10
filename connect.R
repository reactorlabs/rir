rir.viz("http://127.0.0.1:3011")
f <- function(a, b) {
    res = a + b
    res
}

invisible(rir.compile(f))

f(1, 2)
