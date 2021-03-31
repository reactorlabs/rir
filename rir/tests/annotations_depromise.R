
if (Sys.getenv("PIR_ENABLE_ANNOTATIONS") == "1") {
  print("test only works when annotations are not enabled by default")
  q()
}

f <- function(a, ...) {

    #a is supposed to be an unwrapped value
    performStep(4)

    a  # this makes sure a's evaluation is not causing any further effects.
    # this should be a Force(value) -> value

    performStep(5)
    a + 1L

}
f <- rir.annotateDepromised(f)


performStep <- function(n) {
    steps  <<- c(steps, n)
}

g <- function() {

    steps <<- integer();
    performStep(1)

    result <- f({  performStep(2);  6L }, {  performStep(3);  } )
    stopifnot(result == 7L)
    print(steps)
    stopifnot(steps == c(1, 2, 3,4,5))

}

g() # call to f will be interpreted
rir.compile(g)
g()
g()
g()
g()
g()
