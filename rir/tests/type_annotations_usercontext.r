f <- function(x) x + 1

c <- .Call("rirCreateSimpleIntContext")
rir.setUserContext(f, c)

stopifnot(f(2L) == 3) # should run

hasRaisedError <- FALSE
tryCatch({
    f(2) #should fail
}, error = function(error_condition) {
    hasRaisedError <- TRUE
})
stopifnot(!hasRaisedError)



