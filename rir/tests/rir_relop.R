f <- rir.compile(function() {
    stopifnot(1+2 == 3);
    a <- 22;
    b <- 44;
    stopifnot(a+b == 66);
    stopifnot((c(1,2,3) < c(3,2,1)) == c(TRUE, FALSE, FALSE));
})
f()
