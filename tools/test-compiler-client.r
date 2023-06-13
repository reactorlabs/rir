# Small closure (pir_regression.R)
f <- pir.compile(rir.compile(function(a) a(b=1, 2)))
# Memoized
f <- pir.compile(rir.compile(function(a) a(b=1, 2)))
# Memoized again
f <- pir.compile(rir.compile(function(a) a(b=1, 2)))

# Another small closure with a promise
foo <- function(x) {
   y <- x
   function() {
       y <- y + 1
       y
   }
}

stopifnot(pir.check(foo, NoExternalCalls, warmup=function(f) {f(1);f(2)}))

# Medium closure with nested closures (pir_check.R)
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

stopifnot(pir.check(mandelbrot, NoExternalCalls, NoPromise, warmup=function(f) {f(13);f(27)}))

# Memoized
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
stopifnot(pir.check(mandelbrot, NoExternalCalls, NoPromise, warmup=function(f) {f(13);f(27)}))

# Many closures (pir_regression6.R)
lsNamespaceInfo <- function(ns, ...) {
    ns <- asNamespace(ns, base.OK = FALSE)
    ls(..., envir = get(".__NAMESPACE__.", envir = ns, inherits = FALSE))
}
allinfoNS <- function(ns) sapply(lsNamespaceInfo(ns), getNamespaceInfo, ns=ns)
utils::str(allinfoNS("stats"))
utils::str(allinfoNS("stats4"))

# Kill the server
rir.killCompilerServer()