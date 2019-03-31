# Copied / cross-validated from pir_tests

stopifnot(pir.check(function(x, y) print("Test"), IsPirCompilable))
stopifnot(!pir.check(function(x = 4) {
  print("PIR doesn't support default args")
}, IsPirCompilable))
stopifnot(pir.check(function() 42L, Returns42L))
stopifnot(pir.check(function() {
  f <- function() 42L
  f()
}, Returns42L))
stopifnot(pir.check(function() {
  f <- function(val) (function(x) x)(val)
  f(42L)
}, Returns42L))
stopifnot(pir.check(function() {
  f <- function(x) x
  f(42L)
}, Returns42L))
stopifnot(pir.check(function() {
  y <- 42L
  t <- FALSE
  if (t)
    x <- y
  else
    x <- y
  x
}, Returns42L))
stopifnot(pir.check(function() {
  x <- 0
  f <- function() x <<- 42L
  f()
  x
}, Returns42L))
stopifnot(pir.check(function() {
  123
}, NoEnv))
stopifnot(pir.check(function(depth) {
  if (depth == 0)
    1
  else
    0
}, NoEnvSpec))

xxx <- 12
stopifnot(pir.check(function() {
  1 + xxx
}, NoEnvForAdd, warmup=list()))

stopifnot(pir.check(function() {
  1 + yyy
}, NoEnvForAdd))
stopifnot(pir.check(function(x) {
  y <- 2
}, NoStore))
stopifnot(pir.check(function(x) {
  if (x)
    y <- 1
  else
    y <- 2
  y
}, NoStore))
stopifnot(pir.check(function(x) {
  y <- 1
  y <- 2
  leak()
}, NoStore))
stopifnot(!pir.check(function(x) {
  leak()
  y <- 1
  y <- 2
}, NoStore))
stopifnot(pir.check(function(x) {
  leak()
  y <- 1
  y <- 2
}, NoStSuper))
stopifnot(pir.check(function(x) {
  a <- 1
  (function() a <<- 1)()
}, NoStSuper))
stopifnot(pir.check(function(x) {
  a <- 1
  (function() a <<- 1)()
}, NoStore))
stopifnot(!pir.check(function(x) {
  (function() a <<- 1)()
}, NoStSuper))
stopifnot(!pir.check(function(x) {
  (function() a <<- 1)()
}, NoStore))
stopifnot(pir.check(function(x) {
  a <- 1
  (function() {
     a <<- 1
     asdf()
  })()
}, NoStSuper))
stopifnot(!pir.check(function(x) {
  a <- 1
  asdf()
  (function() a <<- 1)()
}, NoStSuper))
stopifnot(pir.check(function() {
  a <- FALSE
  if (a)
    q <- 1
  else {
    if (a)
      q <- 3 
    else 
      q <- 2
  }
  q
}, NoLoad))
stopifnot(!pir.check(function(a) {
  if (a)
    q <- 1
  else {
    if (a)
      q <- 3 
    else 
      q <- 2
  }
  q
}, NoLoad))
stopifnot(pir.check(function() {
  f <- function() 42L
  (function(x) x())(f)
}, Returns42L))
stopifnot(pir.check(function() {
  a <- function() 41L
  b <- function() 1L
  f <- function(x, y) x() + y
  f(a, b())
}, Returns42L))
stopifnot(pir.check(function() {
  x <- function() 32
  y <- function() 31
  z <- 1
  f <- function(a, b, c) {
    if (a() == (b + c))
      42L
  }
  f(x, y(), z)
}, NoEnv))

mandelbrot <- function() {
    size = 30
    sum = 0
    byteAcc = 0
    bitNum  = 0
    y = 0
    while (y < size) {
      ci = (2.0 * y / size) - 1.0
      x = 0
      while (x < size) {
        zr   = 0.0
        zrzr = 0.0
        zi   = 0.0
        zizi = 0.0
        cr = (2.0 * x / size) - 1.5
        z = 0
        notDone = TRUE
        escape = 0
        while (notDone && (z < 50)) {
          zr = zrzr - zizi + cr
          zi = 2.0 * zr * zi + ci
          zrzr = zr * zr
          zizi = zi * zi
          if ((zrzr + zizi) > 4.0) {
            notDone = FALSE
            escape  = 1
          }
          z = z + 1
        }
        byteAcc = bitwShiftL(byteAcc, 1) + escape
        bitNum = bitNum + 1
        if (bitNum == 8) {
          sum = bitwXor(sum, byteAcc)
          byteAcc = 0
          bitNum  = 0
        } else if (x == (size - 1)) {
          byteAcc = bitwShiftL(byteAcc, 8 - bitNum)
          sum = bitwXor(sum, byteAcc)
          byteAcc = 0
          bitNum  = 0
        }
        x = x + 1
      }
      y = y + 1
    }
    return (sum)
}
# This can't be run if PIR_MAX_INPUT_SIZE is too low
stopifnot(tryCatch({
  pir.check(mandelbrot, NoExternalCalls, warmup=list())
}, warning = function(w) {
  cat("Couldn't run:", conditionMessage(w), "\n")
  conditionMessage(w) == "pir check failed: couldn't compile" ||
  conditionMessage(w) == "R JIT disabled, this will prevent some optimizations"
}))

# New tests

stopifnot(pir.check(function() {
  x <- 1
  while (x < 10)
    x <- x + 1
  x
}, NoLoad, NoStore))
stopifnot(pir.check(function(n) {
  x <- 1
  while (x < n)
    x <- x + 1
  x
}, NoLoad, NoStore, warmup=list(10)))
