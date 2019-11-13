jitOn <- as.numeric(Sys.getenv("R_ENABLE_JIT", unset=2)) != 0
jitOn <- jitOn && (Sys.getenv("PIR_ENABLE", unset="on") == "on")

if (!jitOn)
  quit()

# Sanity check for loop peeling, and testing that enabling/disabling works
# These loop peeling tests may be a bit brittle.
# Loop peeling should be enabled by default
stopifnot(pir.check(
  f <- function(x) {
    i <- 0
    while (i < 5) {
      i <- i + x
    }
  }, TwoAdd, warmup=function(f) f(2)))
rir.disableLoopPeeling()
stopifnot(pir.check(
  f <- function(x) {
    i <- 0
    while (i < 5) {
      i <- i + x
    }
  }, OneAdd, warmup=function(f) f(2)))
rir.enableLoopPeeling()
stopifnot(pir.check(
  f <- function(x) {
    i <- 0
    while (i < 5) {
      i <- i + x
    }
  }, TwoAdd, warmup=function(f) f(2)))

# Copied / cross-validated from pir_tests

# This checks that loop-invariant hoisting is working, but it's a bit brittle,
# and it requires loop peeling to be disabled.
stopifnot(pir.check(
  f <- function(){
    j <- 0
    while (j < 2) {
      vector("integer",0)
      j <- j + 1
    }
  }, OneLdVar, warmup=function(f) f()))


stopifnot(
  pir.check(function() {
      for (i in 1:5) {
          a <- c(1)
      }
      a
  }, OneLdFun))

stopifnot(
  pir.check(function() {
      balls = c(1,2,3,4)
      for (i in 1:3){
          balls[[i]] = 0 
      }
      balls[[2]]
  }, OneLdVar, warmup=function(f) f()))  

stopifnot(pir.check(function(x, y) print("Test"), IsPirCompilable))
stopifnot(pir.check(function(x = 4) {
  print("PIR does support default args")
}, IsPirCompilable))
stopifnot(!pir.check(function(...) {
  foo(...);
  print("PIR does support dotdotdot")
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
}, NoEnvSpec, warmup=function(f){cat(".\n"); f(0)}))
seed <- 1   
stopifnot(pir.check(function(a) {
  seed <<- a
}, NoEnvSpec))
stopifnot(pir.check(function() {
  a <- (seed * 1309) + 13849
  seed <<- bitwAnd(a, 65535)
  seed
}, NoEnvSpec, warmup=function(f){f()}))
xxx <- 12
stopifnot(pir.check(function() {
  1 + xxx
}, NoEnvForAdd, warmup=function(f) f()))
yyy = 0
stopifnot(pir.check(function() {
  1 + yyy
}, NoEnvForAdd, warmup=function(f) f()))
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
stopifnot(pir.check(function(x) {
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
  leak()
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
stopifnot(pir.check(function(a) {
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
}, Returns42L, warmup=function(f)f()))
stopifnot(pir.check(function() {
  x <- function() 32
  y <- function() 31
  z <- 1
  f <- function(a, b, c) {
    if (a() == (b + c))
      42L
  }
  f(x, y(), z)
}, NoEnv, warmup=function(f)f()))

mandelbrot <- function(size) {
    size = size
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
# TODO: FIXXXXX
stopifnot(
  pir.check(mandelbrot, NoExternalCalls, NoPromise, NoStore, warmup=function(f) {f(13);f(27)})
)

# New tests
stopifnot(pir.check(function() {
  x <- 1
  while (x < 10)
    x <- x + 1
  x
}, NoLoad, NoStore, warmup=function(f)f()))
stopifnot(pir.check(function(n) {
  x <- 1
  while (x < n)
    x <- x + 1
  x
}, NoLoad, NoStore, warmup=function(f)f(10)))

# Negative Test

stopifnot(!pir.check(function() x(), NoExternalCalls))

# Numeric effect removal
# Testing NoEq and OneEq
stopifnot(pir.check(function(x) x + 3, NoEq))
stopifnot(!pir.check(function(x) x == 3, NoEq))
stopifnot(pir.check(function(x) x == 3, OneEq))
stopifnot(!pir.check(function(x) x == 3 || x == 4, NoEq))
stopifnot(!pir.check(function(x) x == 3 || x == 4, OneEq))
# Ok

# More constantfolding

stopifnot(pir.check(function() {
  if (!FALSE)
    42L
  else
    41L
}, Returns42L))
stopifnot(pir.check(function() {
  if (!TRUE)
    41L
  else
    42L
}, Returns42L))
stopifnot(pir.check(function() {
  x <- FALSE
  y <- 41L
  if (!x == TRUE)
    y + 1L
  else
    y - 1L
}, Returns42L))
stopifnot(!pir.check(function() {
  x <- NA
  y <- 41L
  if (!x == NA)
    y + 1L
  else
    y - 1L
}, Returns42L))
stopifnot(pir.check(function() {
  x <- NA
  y <- 41L
  if (!x == NA)
    y + 1L
  else
    y - 1L
}, NoEq))
stopifnot(pir.check(function(x) {
  if ((x == 1) == TRUE)
    5
  else
    4
}, OneEq, warmup=function(f)f(3)))
stopifnot(pir.check(function(x) {
  if ((x == 1) == FALSE)
    5
  else
    4
}, OneEq, warmup=function(f)f(4L)))
stopifnot(pir.check(function(x, y) {
  a <- y == 1 # This is the one eq
  (x == 1) == NA
}, OneEq, warmup=function(f)f(5.7, "")))
# Relies on better visibility
# stopifnot(pir.check(function(x) !!!!!x, OneNot, warmup=function(f)f(1)))
# Testing NoAsInt itself
stopifnot(!pir.check(function(n) {
  x <- 0
  for (i in 1:n)
    x <- x + i
  x
}, NoAsInt))
# Ok
stopifnot(pir.check(function() {
  x <- 0
  for (i in 1:10)
    x <- x + i
  x
}, NoAsInt))
                     
# More dead instruction removal
stopifnot(!pir.check(function(x) {
  x == 4
  x
}, NoEq))
stopifnot(pir.check(function(x) {
  x == 4
  x
}, NoEq, warmup=function(f)f(5)))
stopifnot(pir.check(function(x, y) {
  x == 3+7i
  y == NA
  x + y
}, NoEq, warmup=function(f)f(5L, 2L)))

## Inline promises even when they escape only because of deopt 
nbodyPrologue <- function(args) {
  n = if (length(args)) 20 else 1000L
  n
}

stopifnot(
  pir.check(function(arg) {nbodyPrologue(arg)}, NoExternalCalls, NoPromise, NoEnvSpec, warmup=function(f) {f(10);f(10)})
)

## Start by proving properties on simplified versions of bounce
seed <- NaN
nextRandom <- function() {
  a <- (seed * 1309) + 13849
  seed <<- bitwAnd(a, 65535)
  seed
}
simplifiedBounceInit <- function () {
    ballCount = 2
    balls     = vector("list", length = ballCount)
    for (i in 1:ballCount) {
        random1 = nextRandom()
        balls[[i]] = c(random1 %% 500)
    }
   return(balls[[1]])
}

stopifnot(
  pir.check(simplifiedBounceInit, NoEnvSpec, NoPromise, warmup=function(f) {f()})
)