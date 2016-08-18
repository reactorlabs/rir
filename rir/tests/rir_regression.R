f1 <- rir.compile(function() {
    a <- 1
    class(a) <- "asdf"
    b <- a
    class(b) <- NULL
    class(a)
})

stopifnot(f1() == "asdf")

f2 <- rir.compile(function() {
    a <- c(1)
    a <- c(quote(a))
    a[[1]] <- quote(b)
    a[[1]]
})

stopifnot(f2() == quote(b))
