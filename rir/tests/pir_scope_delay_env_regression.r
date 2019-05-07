# In this regression the diagonal function was primed with
# the first else branch being dead. Then we compile with
# the assumption that n is not missing, which will kill
# the then branch.
# this leads to a function that directly deoptimizes (which
# is intended to pick up typefeedback). But scope analysis
# environment delay optimization would create an environment
# with a fudged missing tag on the binding, since it did not
# consider the difference between stvar and starg (the later
# preserves missing flag.

Diagonal = function (n, x = NULL) 
{
    n <- if (missing(n)) 
        length(x)
    else {
        stopifnot(length(n) == 1, n == as.integer(n), n >= 0)
        as.integer(n)
    }
    if (missing(x)) 
        c(1,2,3)
    else {
        lx <- length(x)
        lx.1 <- lx == 1L
        stopifnot(lx.1 || lx == n)
        if (is.logical(x)) 
            cl <- "ldiMatrix"
        else if (is.numeric(x)) {
            cl <- "ddiMatrix"
            x <- as.numeric(x)
        }
        else if (is.complex(x)) {
            cl <- "zdiMatrix"
        }
        else stop("'x' has invalid data type")
        if (lx.1 && !is.na(x) && x == 1) 
            new(cl, Dim = c(n, n), diag = "U")
        else new(cl, Dim = c(n, n), diag = "N", x = if (lx.1) 
            rep.int(x, n)
        else x)
    }
}

t <- function() {
  Diagonal()
  Diagonal()
  Diagonal(1)
}

pir.compile(rir.compile(t))
t()
