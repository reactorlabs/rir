library(Matrix)
source(system.file("test-tools.R", package = "Matrix"))

testxyz <- function() {
I <- i1 <- I1 <- Diagonal(1)
I1[1,1] <- i1[1, ] <- I [ ,1] <- NA
print(I)
print(i1)
print(I1)
stopifnot(identical3(I,i1,I1))
}

testxyz()

(tr <- Matrix(cbind(1,0:1)))
testxyz()
sL <- Matrix(, 3,4, sparse=TRUE)# -> "lgC"
testxyz()
trS <- Matrix(tr, sparse=TRUE)# failed in 0.9975-11
testxyz()
stopifnotValid(tr, "triangularMatrix"); stopifnotValid(trS, "triangularMatrix")
testxyz()
stopifnot(all(is.na(sL@x)), ## not yet:  all(is.na(sL)),
          !any(sL, na.rm=TRUE), all(!sL, na.rm=TRUE),
          validObject(Matrix(c(NA,0), 4, 3, byrow = TRUE)),
          validObject(Matrix(c(NA,0), 4, 4)))
testxyz()
stopifnotValid(Matrix(c(NA,0,0,0), 4, 4), "sparseMatrix")
testxyz()

I <- i1 <- I1 <- Diagonal(1)
I1[1,1] <- i1[1, ] <- I [ ,1] <- NA
print(I)
print(i1)
print(I1)
stopifnot(identical3(I,i1,I1))
