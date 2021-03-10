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


# Here we slightly depart from gnur semantics.
# See Instruction::mayObserveContext exception for Force
# stopifnot(f(bad()) == "a")
g <- function(a) a
f <- function(b) g(b)
bad = function() {e = sys.frame(-1); ls(envir=e)}
f(bad())
f(bad())
f(bad())
f(bad())
f(bad())

f0 <- function() {
  for (i in 1:10)
    last <- i;
  last
}
stopifnot(f0() == 10)
stopifnot(f0() == 10)
stopifnot(f0() == 10)
stopifnot(f0() == 10)
stopifnot(f0() == 10)
stopifnot(f0() == 10)




f <- function() g(1,2)
g <- function(a,b) h(a,b,1)
h <- function(...) {
  x <- function(...) c(...)
  forceAndCall(3, x, ...)
}
stopifnot(identical(f(), c(1,2,1)))
stopifnot(identical(f(), c(1,2,1)))
stopifnot(identical(f(), c(1,2,1)))
stopifnot(identical(f(), c(1,2,1)))
stopifnot(identical(f(), c(1,2,1)))
stopifnot(identical(f(), c(1,2,1)))
