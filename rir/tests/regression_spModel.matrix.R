library(Matrix)

## This is example(sp....) -- much extended

mEQ <- function(x,y, ...) {
    ## first drop columns from y  which are all 0 :
    if(any(i0 <- colSums(abs(x)) == 0)) {
        message(gettextf("x had  %d  zero-columns", sum(i0)))
        x <- x[, !i0, drop=FALSE]
    }
    if(any(i0 <- colSums(abs(y)) == 0)) {
        message(gettextf("y had  %d  zero-columns", sum(i0)))
        y <- y[, !i0, drop=FALSE]
    }
    isTRUE(all.equal(x,y, tolerance =0, ...))
}

##' Is  sparse.model.matrix() giving the "same" as dense model.matrix() ?
##'
##' @return logical
##' @param frml formula
##' @param dat data frame
##' @param showFactors
##' @param ...
isEQsparseDense <- function(frml, dat,
                            showFactors = isTRUE(getOption("verboseSparse")), ...)
{
    ## Author: Martin Maechler, Date: 21 Jul 2009
    stopifnot(inherits(frml, "formula"), is.data.frame(dat))
    if(showFactors)
        print(attr(terms(frml, data=dat), "factors"))
    smm <- sparse.model.matrix(frml, dat, ...)
     mm <-        model.matrix(frml, dat, ...)
    sc <- smm@contrasts
    mEQ(as(smm, "generalMatrix"), Matrix(mm, sparse=TRUE)) &
     identical(smm@assign, attr(mm, "assign")) &
     (if(is.null(mc <- attr(mm, "contrasts"))) length(sc) == 0 else identical(sc, mc))
}

### ------------ all the "datasets" we construct for use -------------
dd <- data.frame(a = gl(3,4), b = gl(4,1,12))# balanced 2-way
(dd3 <- cbind(dd, c = gl(2,6), d = gl(3,8)))
dd. <- dd3[- c(1, 13:15, 17), ]
set.seed(17)
dd4 <- cbind(dd, c = gl(2,6), d = gl(8,3))
dd4 <- cbind(dd4, x = round(rnorm(nrow(dd4)), 1))
dd4 <- dd4[- c(1, 13:15, 17), ]
##-> 'd' has unused levels
dM <- dd4
dM$X <- outer(10*rpois(nrow(dM), 2), 1:3)
dM$Y <- cbind(pmax(0, dM$x - .3), floor(4*rnorm(nrow(dM))))
str(dM)# contains *matrices*

options("contrasts") # the default:  "contr.treatment"
op <- options(sparse.colnames = TRUE) # for convenience

stopifnot(identical(## non-sensical, but "should work" (with a warning each):
		    sparse.model.matrix(a~ 1, dd),
		    sparse.model.matrix( ~ 1, dd)))
sparse.model.matrix(~ a + b, dd, contrasts = list(a="contr.sum"))
sparse.model.matrix(~ a + b, dd, contrasts = list(b="contr.SAS"))
xm <-  sparse.model.matrix(~ x, dM) # {no warning anymore ...}
dxm <- Matrix(model.matrix(~ x, dM), sparse=TRUE)
stopifnot(is(xm, "sparseMatrix"), mEQ(as(xm,"generalMatrix"), dxm))

## Sparse method is equivalent to the traditional one :
stopifnot(isEQsparseDense(~ a + b, dd),
          suppressWarnings(isEQsparseDense(~ x, dM)),
          isEQsparseDense(~ 0 + a + b, dd),
	  identical(sparse.model.matrix(~  0 + a + b, dd),
		    sparse.model.matrix(~ -1 + a + b, dd)),
          isEQsparseDense(~ a + b, dd, contrasts = list(a="contr.sum")),
          isEQsparseDense(~ a + b, dd, contrasts = list(a="contr.SAS")),
	  ## contrasts as *functions* or contrast *matrices* :
	  isEQsparseDense(~ a + b, dd,
			  contrasts = list(a=contr.sum, b=contr.treatment(4))),
	  isEQsparseDense(~ a + b, dd, contrasts =
			  list(a=contr.SAS(3),# << ok after 'contrasts<-' update
                               b = function(n, contr=TRUE, sparse=FALSE)
                               contr.sum(n=n, contr=contr, sparse=sparse))))

sm <- sparse.model.matrix(~a * b, dd,
                          contrasts = list(a= contr.SAS(3, sparse = TRUE)))
