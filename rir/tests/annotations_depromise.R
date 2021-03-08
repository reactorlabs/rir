if (Sys.getenv("PIR_DISABLE_ANNOTATIONS") == "1") {
  print("test only works with annotations enabled")
  q()
}


f <- function(a) {

    #a is supposed to be an unwrapped value
    performStep(3)

    a  # this makes sure a's evaluation is not causing any further effects.
    # this should be a Force(value) -> value

    performStep(4)
    a + 1L

}
rir.compile(f)
rir.markFunction(f, DepromisedArgs=TRUE)


performStep <- function(n) {
    steps  <<- c(steps, n)
}

g <- function() {

    steps <<- integer();
    performStep(1)

    result <- f({  performStep(2);  6L })
    stopifnot(result == 7L)
    stopifnot(steps == c(1, 2, 3,4))

}

g() # call to f will be interpreted
rir.compile(g)
g()
g()
g()
g()
g()
