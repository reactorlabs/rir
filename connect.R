rir.viz("http://127.0.0.1:3011")

g <- function(a) {
    a * a
}

f <- function(a, b) {
    res = a + b
    g(res)
}

invisible(rir.compile(f))

f(1, 2)
