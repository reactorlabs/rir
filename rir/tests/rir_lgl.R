f <- rir.compile(function() {
    a1 <- TRUE && TRUE
    stopifnot(a1 == TRUE)
    a2 <- TRUE && FALSE
    stopifnot(a2 == FALSE)
    a3 <- TRUE && NA
    stopifnot(is.na(a3))
    a4 <- FALSE && TRUE
    stopifnot(a4 == FALSE)
    a5 <- FALSE && FALSE
    stopifnot(a5 == FALSE)
    a6 <- FALSE && NA
    stopifnot(a6 == FALSE)
    a7 <- NA && TRUE
    stopifnot(is.na(a7))
    a8 <- NA && FALSE
    stopifnot(a8 == FALSE)
    a9 <- NA && NA
    stopifnot(is.na(a9))
    a10 <- TRUE || TRUE
    stopifnot(a10 == TRUE)
    a11 <- TRUE || FALSE
    stopifnot(a11 == TRUE)
    a12 <- TRUE || NA
    stopifnot(a12 == TRUE)
    a13 <- FALSE || TRUE
    stopifnot(a13 == TRUE)
    a14 <- FALSE || FALSE
    stopifnot(a14 == FALSE)
    a15 <- FALSE || NA
    stopifnot(is.na(a15))
    a16 <- NA || TRUE
    stopifnot(a16 == TRUE)
    a17 <- NA || FALSE
    stopifnot(is.na(a17))
    a18 <- NA || NA
    stopifnot(is.na(a18))
    a19 <- !TRUE
    stopifnot(a19 == FALSE)
    a20 <- !FALSE
    stopifnot(a20 == TRUE)
    a21 <- !NA
    stopifnot(is.na(a21))
    a22 <- !((1:5 %% 2) == 0)
    stopifnot(a22 == c(TRUE, FALSE, TRUE, FALSE, TRUE))
})

f()
