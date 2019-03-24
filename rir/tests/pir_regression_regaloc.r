f <- function() {
  if ("")
    x <- 1
  else
    x <- 2

  if ("")
    y <- 3
  else
    y <- 5

  while("") {
    x && y
    x <- 10
  }
}
pir.compile(rir.compile(f))
