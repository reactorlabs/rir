f <- function() {
  f <- function(q) {
    e <<- environment()
    a <- TRUE
    q
  }

  stopifnot(f(ls(envir=e)) == c("a", "q"))
  stopifnot(f(ls(envir=e)) == c("a", "q"))
  stopifnot(f(ls(envir=e)) == c("a", "q"))
  stopifnot(f(ls(envir=e)) == c("a", "q"))
}
for(i in 1:10)
  f()

f <- function() {
  f <- function(i) {
    g(i)
  }
  g <- function(i) {
    ls(envir=sys.frame(-i))
  }
  r2 = f(2)
  r1 = f(1)
  stopifnot(r1 == c("i"))
  stopifnot(r2 == c("f", "g"))
  r2 = f(2)
  r1 = f(1)
  stopifnot(r1 == c("i"))
  stopifnot(r2 == c("f", "g", "r1", "r2"))
  r2 = f(2)
  r1 = f(1)
  stopifnot(r1 == c("i"))
  stopifnot(r2 == c("f", "g", "r1", "r2"))
}
for(i in 1:10)
  f()
