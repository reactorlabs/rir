warnifnot <- function(x) {
    text <- deparse(substitute(x))
    if (!x) warning(paste(text, "failed"))
}

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

warnifnot(pir.check(foo, NoExternalCalls, warmup=function(f) {f(1);f(2)}))

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

warnifnot(pir.check(mandelbrot, NoExternalCalls, NoPromise, warmup=function(f) {f(13);f(27)}))

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
warnifnot(pir.check(mandelbrot, NoExternalCalls, NoPromise, warmup=function(f) {f(13);f(27)}))

# Many closures (reg-tests-1c.R)
## merge.dendrogram(), PR#15648
mkDend <- function(n, lab, method = "complete",
                   ## gives *ties* often:
		   rGen = function(n) 1+round(16*abs(rnorm(n)))) {
    stopifnot(is.numeric(n), length(n) == 1, n >= 1, is.character(lab))
    a <- matrix(rGen(n*n), n, n)
    colnames(a) <- rownames(a) <- paste0(lab, 1:n)
    .HC. <<- hclust(as.dist(a + t(a)), method=method)
    as.dendrogram(.HC.)
}

## recursive dendrogram methods and deeply nested dendrograms
op <- options(expressions = 999)# , verbose = 2) # -> max. depth= 961
set.seed(11); d <- mkDend(1500, "A", method="single")
rd <- reorder(d, nobs(d):1)
## Error: evaluation nested too deeply: infinite recursion .. in R <= 3.2.3
stopifnot(is.leaf(r1 <- rd[[1]]),    is.leaf(r2 <- rd[[2:1]]),
	  attr(r1, "label") == "A1458", attr(r2, "label") == "A1317")
options(op)# revert

## recursive dendrogram methods and deeply nested dendrograms
op <- options(expressions = 999)# , verbose = 2) # -> max. depth= 961
set.seed(11); d <- mkDend(1500, "A", method="single")
print(d[[1]])
rd <- reorder(d, nobs(d):1)
print(rd[[1]])
## Error: evaluation nested too deeply: infinite recursion .. in R <= 3.2.3
stopifnot(is.leaf(r1 <- rd[[1]]),    is.leaf(r2 <- rd[[2:1]]),
	  attr(r1, "label") == "A1458", attr(r2, "label") == "A1317")
options(op)# revert

## recursive dendrogram methods and deeply nested dendrograms
op <- options(expressions = 999)# , verbose = 2) # -> max. depth= 961
set.seed(11); d <- mkDend(1500, "A", method="single")
print(d[[1]])
rd <- reorder(d, nobs(d):1)
print(rd[[1]])
## Error: evaluation nested too deeply: infinite recursion .. in R <= 3.2.3
stopifnot(is.leaf(r1 <- rd[[1]]),    is.leaf(r2 <- rd[[2:1]]),
	  attr(r1, "label") == "A1458", attr(r2, "label") == "A1317")
options(op)# revert

## recursive dendrogram methods and deeply nested dendrograms
op <- options(expressions = 999)# , verbose = 2) # -> max. depth= 961
set.seed(11); d <- mkDend(1500, "A", method="single")
print(d[[1]])
rd <- reorder(d, nobs(d):1)
print(rd[[1]])
## Error: evaluation nested too deeply: infinite recursion .. in R <= 3.2.3
stopifnot(is.leaf(r1 <- rd[[1]]),    is.leaf(r2 <- rd[[2:1]]),
	  attr(r1, "label") == "A1458", attr(r2, "label") == "A1317")
options(op)# revert

# Many more closures (pir_regression6.R)
lsNamespaceInfo <- function(ns, ...) {
    ns <- asNamespace(ns, base.OK = FALSE)
    ls(..., envir = get(".__NAMESPACE__.", envir = ns, inherits = FALSE))
}
allinfoNS <- function(ns) sapply(lsNamespaceInfo(ns), getNamespaceInfo, ns=ns)
utils::str(allinfoNS("stats"))
utils::str(allinfoNS("stats4"))

# Kill the server (named "servers" because it kills all connected servers,
# but there is only one in this case)
rir.killCompilerServers()
