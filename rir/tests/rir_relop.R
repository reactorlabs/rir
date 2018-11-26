f <- rir.compile(function() {
    stopifnot(1+2 == 3);
    a <- 22;
    b <- 44;
    stopifnot(a+b == 66);
    stopifnot(-1:3 == c(-1, 0, 1, 2, 3));
    stopifnot((c(1,2,3) < c(3,2,1)) == c(TRUE, FALSE, FALSE));

    stopifnot((c(1,2,3) <= c(3,2,1)) == c(TRUE, TRUE, FALSE));
    stopifnot((c(1,2,3) > c(3,2,1)) == c(FALSE, FALSE, TRUE));
    stopifnot((c(1,2,3) >= c(3,2,1)) == c(FALSE, TRUE, TRUE));
    stopifnot((c(1,2,3) == c(3,2,1)) == c(FALSE, TRUE, FALSE));
    stopifnot((c(1,2,3) != c(3,2,1)) == c(TRUE, FALSE, TRUE));
})
f()
