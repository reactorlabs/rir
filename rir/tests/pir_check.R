jitOn <- as.numeric(Sys.getenv("R_ENABLE_JIT", unset=2)) != 0
jitOn <- jitOn && (Sys.getenv("PIR_ENABLE", unset="on") == "on")

if (!jitOn)
  quit()

if (Sys.getenv("PIR_GLOBAL_SPECIALIZATION_LEVEL") != "")
  q()

deoptChaos <- as.numeric(Sys.getenv("PIR_DEOPT_CHAOS", unset=0))

# If this test runs in the with deopt chaos, weird things might
# happen (cf. #1258): a function might be compiled with the
# chaos on but run in a context with chaos off making the
# assert in deoptChaosTriggerImpl fail.
if (deoptChaos != 0) {
  warning("skipping due to PIR_DEOPT_CHAOS=", deoptChaos, " set")
  q()
}

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


if (Sys.getenv("PIR_OPT_LEVEL") != "" && as.integer(Sys.getenv("PIR_OPT_LEVEL")) < 1) {
  warning("skipping rest of test since opt level < 1")
  q()
}

# Copied / cross-validated from pir_tests

# This checks that loop-invariant hoisting is working, but it's a bit brittle,
# and it requires loop peeling to be disabled.
stopifnot(pir.check(f <- function(){
  j <- 0
  while (j < 2) {
    vector("integer",0)
    j <- j + 1
  }
}, OneLdVar, warmup=function(f) f()))

# Loop hoisting + simple range constantfold dead branch removal
stopifnot(pir.check(function() {
  for (i in 1:5) {
      a <- c(1)
  }
  a
}, NoLdFun, warmup=function(f) f()))

# Loop hoisting + simple range non-constant dead branch removal
stopifnot(pir.check(function(b) {
  for (i in b:5) {
      a <- c(1)
  }
  a
}, OneLdVar, warmup=function(f) f(1)))

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


if (Sys.getenv("PIR_OPT_LEVEL") != "" && as.integer(Sys.getenv("PIR_OPT_LEVEL")) < 2) {
  warning("skipping rest of test since opt level < 2")
  q()
}


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

if (Sys.getenv("PIR_OPT_LEVEL") != "" && as.integer(Sys.getenv("PIR_OPT_LEVEL")) < 3) {
  warning("skipping rest of test since opt level < 3")
  q()
}


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

# TODO: FIXXXXX
stopifnot(
  pir.check(mandelbrot, NoExternalCalls, NoPromise, warmup=function(f) {f(13);f(27)})
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
# Testing simple range dead branch removal
# This one gets constantfolded...
stopifnot(pir.check(function() {
  x <- 0
  for (i in 1:10)
    x <- x + i
  x
}, NoColon))
# ...but not these, we need to call the function more often for test feedback
stopifnot(pir.check(function(n) {
  x <- 0
  for (i in 1:n)
    x <- x + i
  x
}, NoColon, warmup=function(f){f(10); f(10)}))
stopifnot(pir.check(function(n) {
  x <- 0
  for (i in n:10)
    x <- x + i
  x
}, NoColon, warmup=function(f){f(10); f(10)}))
a <- 1
b <- 10
stopifnot(pir.check(function(a, b) {
  x <- 0
  for (i in a:b)
    x <- x + i
  x
}, NoColon, warmup=function(f) {f(1, 10); f(1, 10)}))
stopifnot(pir.check(function(a, b) {
  x <- 0
  for (i in a:b)
    x <- x + i
  x
}, NoColon, warmup=function(f) {f(a, 10); f(a, 10)}))
stopifnot(pir.check(function(a, b) {
  x <- 0
  for (i in a:b)
    x <- x + i
  x
}, NoColon, warmup=function(f) {f(1, b); f(1, b)}))
stopifnot(pir.check(function(a, b) {
  x <- 0
  for (i in a:b)
    x <- x + i
  x
}, NoColon, warmup=function(f) {f(a, b); f(a, b)}))
a <- factor(a)
b <- factor(b)
stopifnot(!pir.check(function(a, b) {
  x <- 0
  for (i in a:b)
    x <- i
  x
}, NoColon, warmup=function(f) {f(a, b); f(a, b)}))

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

emptyFor <- function(n) {
  for (i in 1:n) {

  }
}
stopifnot(pir.check(emptyFor, OneAdd, AnAddIsNotNAOrNaN, warmup=function(f) {f(1000)}))
arg <- 1000
stopifnot(pir.check(emptyFor, OneAdd, AnAddIsNotNAOrNaN, warmup=function(f) {f(arg)}))



f <- function() g(1,2)
g <- function(a,b) h(a,b,1)
h <- function(a,b,c) {
  x <- function(a,b,c) c(a,b,c);
  forceAndCall(3, x, a,b,c)
}
stopifnot(pir.check(f, NoExternalCalls, warmup=function(f) {f();f()}))




f <- function() g(1,2)
g <- function(a,b) h(a,b,1)
h <- function(r,s,t) {
  x <- function(...) c(...);
  forceAndCall(3, x, r,s,t)
}
stopifnot(pir.check(f, NoExternalCalls, warmup=function(f) {f();f()}))

f <- function() 1L
g <- function(x) x
h <- function() {
  g(g(g(f()) + g(g(f()))) + g(40L))
}
stopifnot(pir.check(h, NoExternalCalls, Returns42L, warmup=function(h) {h();h()}))

# checks range analysis
f <- function(a,b) if (b > 0) a[b]
stopifnot(pir.check(f, UnboxedExtract, warmup=function(f) f(1,1)))
