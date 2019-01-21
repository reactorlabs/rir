f <- function(a,b,c) nargs()
g <- function() {
  f()
  f(1)
  f(1,2)
  f(1,2,3)
}
h <- function() g()

f <- pir.compile(rir.compile(f))
g <- pir.compile(rir.compile(g))
h <- pir.compile(rir.compile(h))

stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)




f <- function(a,b,c) nargs() + (if (!missing(a)) a else 1)
g <- function() {
  stopifnot(f() == 1)
  stopifnot(f(1) == 2)
  stopifnot(f(1,2) == 3)
  stopifnot(f(1,2,3) == 4)
  f(1,2)
}
h <- function() g()

f <- pir.compile(rir.compile(f))
g <- pir.compile(rir.compile(g))
h <- pir.compile(rir.compile(h))

stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)




f <- function(a,b,c) nargs() + a
g <- function(q) {
  stopifnot(f(q) == 2)
  print (f(q,2))
  stopifnot(f(q,2) == 3)
  stopifnot(f(q,2,3) == 4)
  f(q,2)
}
h <- function() g(1)

f <- pir.compile(rir.compile(f))
g <- pir.compile(rir.compile(g))
h <- rir.compile(h)

stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
