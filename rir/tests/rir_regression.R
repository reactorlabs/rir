f1 <- rir.compile(function() {
    a <- 1
    class(a) <- "asdf"
    b <- a
    class(b) <- NULL
    class(a)
})

stopifnot(f1() == "asdf")

f <- rir.compile(function() {
    b0 <- gl(3,4, labels=letters[1:3])
    bf <- setNames(b0, paste0("o", seq_along(b0)))
    df  <- data.frame(a = 1, B = b0, f = gl(4,3))
})
f()

f <- rir.compile(function() {
a <- list( 1 ); b <- (a[[1]] <- a); stopifnot(identical(b, list( 1 )))
a <- list(x=1); b <- ( a$x  <-  a); stopifnot(identical(b, list(x=1)))
})

rir.disassemble(f)
f()

f <- rir.compile(function() {
    x <- seq(0, 4, length.out = 501)
})
f()


f <- rir.compile(function() {
    sessionInfo()
})
f()

elem_max <- 2

f <- rir.compile(
function (x) 
{
    x_len <- length(x)
    if (x_len == 1L) 
        return(x%/%2L)
    borrow <- (x[[1]] == 1)
    x_start <- borrow + 1L
    x_end <- x_len
    result_index <- 1L
    result <- integer(x_end - x_start + 1L)
    for (x_index in x_start:x_end) {
        d = x[[x_index]] + elem_max * borrow
        result[[result_index]] <- d%/%2
        borrow <- d%%2
        result_index <- result_index + 1L
    }
    return(result)
})
for (i in 1:5000) f(c(1,2,3))

(rir.compile(function(a) {a;a}))(1)
