f <- function(n) {
  i <- 0
  while (i < n) {
    i <- i + 1
  }
  i
}
stopifnot(f(250) == 250)

f <- function(n) {
  x <- 0
  for (i in 1:n) {
    x <- x + i
  }
  x
}
stopifnot(f(100) == 5050)
