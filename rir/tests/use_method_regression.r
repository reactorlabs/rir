for(nm in c(1)) {
    y7 <- function(L) 1/8 + c(9:4, L)
    w1  <- lapply(c(1000, Inf), function(L) wilcox.test( y7(L) ))
    print(w1)
    stopifnot(
        identical(w1  [[1]], w1  [[2]]) # was FALSE ..
    )
}
