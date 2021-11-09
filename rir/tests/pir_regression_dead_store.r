foo <- function() {
  n <- 5
  function() n
}

f <- foo()
f <- foo()
f <- foo()
f <- foo()
f <- foo()
f <- foo()
f <- foo()
f()


foo <- function() {
  n_default <- 5
  function(n = n_default) n
}

f <- foo()
f <- foo()
f <- foo()
f <- foo()
f <- foo()
f <- foo()
f <- foo()
f()


foo <- function() {
  n_default <- 5
  delayedAssign("n", n_default, assign.env = globalenv())
}

foo()
foo()
foo()
foo()
foo()
foo()
foo()
f <- function() n
f()
