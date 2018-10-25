test <- function(x, tests) {
    t1 <- rir.compile(x)
    t2 <- pir.compile(t1)
    for (t in tests)
        stopifnot(t1(t) == t2(t))
}

test(function(x) {
  s <- 0
  for (i in x)
      s <- s+i
  s
}, c(1:10))

test(function(x) {
    s <- ""
    for (i in 1:x)
        s <- cat(i, " ", s)
    s
}, c(1, 0, 100))
