# helpers for testing error/warning
checkError <- function(expr, msg) {
  caught <- FALSE
  env <- environment()
  tryCatch(expr, error=function(e) { assign("caught", TRUE, envir=env); stopifnot(msg == e[1]) })
  stopifnot(caught)
}
checkWarning <- function(expr, msg) {
  caught <- FALSE
  env <- environment()
  tryCatch(expr, warning=function(e) { assign("caught", TRUE, envir=env); stopifnot(msg == e[1]) })
  stopifnot(caught)
}


f1 <- rir.compile(function() {
  switch()
})
checkError(f1(), "'EXPR' is missing")


f2 <- rir.compile(function(x) {
  switch(x)
})
checkWarning(f2(1), "'switch' with no alternatives")
res <- f2(1)
stopifnot(res == NULL)
checkWarning(f2("one"), "'switch' with no alternatives")
stopifnot(f2("one") == NULL)
checkError(f2(NULL), "EXPR must be a length 1 vector")
checkError(f2(c(1, 2)), "EXPR must be a length 1 vector")


f3 <- rir.compile(function(x) {
  switch(x, a=, b=17, 42, "foo")
})
checkError(f3(1), "empty alternative in numeric switch")
checkError(f3("b"), "duplicate 'switch' defaults")
checkError(f3(NULL), "EXPR must be a length 1 vector")
checkError(f3(c(1, 2)), "EXPR must be a length 1 vector")
stopifnot(f3(0) == NULL)
stopifnot(f3(2) == 17)
stopifnot(f3(3) == 42)
stopifnot(f3(4) == "foo")
stopifnot(f3(5) == NULL)


f4 <- rir.compile(function(x) {
  switch(x, a=, b=17, 42, c=, d=, e=20)
})
stopifnot(f4("a") == 17)
stopifnot(f4("b") == 17)
stopifnot(f4("c") == 20)
stopifnot(f4("d") == 20)
stopifnot(f4("e") == 20)
stopifnot(f4("dft") == 42)


f5 <- rir.compile(function(x) {
  switch(x, a=17, "42", b=c(1, 2))
})
stopifnot(f5(1) == 17)
stopifnot(f5(1.2) == 17)
stopifnot(f5(2) == "42")
stopifnot(f5(3) == c(1, 2))
stopifnot(f5(3.002) == c(1, 2))
stopifnot(f5(42) == NULL)
