curve(sin, -2*pi, 3*pi); pu1 <- par("usr")[1:2]
curve(cos, add = NA) # add = NA new in 2.14.0


{
    # dead store elimination regression
    test <- pir.compile(rir.compile(function() {
        a <- 1
        id <- function(x) x
        id(a)
    }))
    
    t <- rir.compile(function() test())
    stopifnot(t() == 1);
}
