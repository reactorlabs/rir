mandelbrot_mini <- function() {
   n <- 2000L
   C <- matrix(0, n, n)
   for (y in 0:(n-1)) {
       C[, y] <- 2 * 0:(n-1) / n - 1.5 + 1i * (2 * y / n - 1)
   }
}

f <- rir.compile(function() mandelbrot_mini())

f()
f()
pir.compile(mandelbrot_mini, debugFlags=pir.debugFlags(PrintPirAfterOpt=TRUE, ShowWarnings=TRUE))
