f1 <- rir.compile(function() {
    a <- 1
    class(a) <- "asdf"
    b <- a
    class(b) <- NULL
    class(a)
})

stopifnot(f1() == "asdf")
