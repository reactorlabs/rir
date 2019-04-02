f <- function(x, y) {
  x %% 2L
}
for (i in 1:10) {
  stopifnot(!is.na(f(i)))
}

bar <- function(x) x + 2
f <- function(x) {
  sum <- 0
  while (x > 0) {
    sum <- sum + x
    x <- x - 1
    bar(sum)
  }
  sum
}
stopifnot(f(10) == 55)

f <- function () {
    bounces <- FALSE
    ball <- c(42)
    names(ball) <- c("")
    for (i in 1:1) {
        if (ball[1] == 42)
            bounces <- TRUE
    }
    bounces
}
stopifnot(f())
