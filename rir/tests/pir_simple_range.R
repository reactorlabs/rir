f <- rir.compile(function(depth) {
    if (depth == 1) {
      1
    } else {
        x <- 0
        for (i in 1:4) {
            x <- x + f(depth - 1)
        }
        x
    }
})
stopifnot(f(4) == 64)

f <- rir.compile(function() {
  x <- 1
  for (i in 1L:x) NULL
})
f()
f()
f()

f <- rir.compile(function() {
    a <- 0
    for (i in 1:1) {
      a <- i
    }
    a
})
print(f())
stopifnot(f() == 1)
pir.compile(f)
print(f())
stopifnot(f() == 1)

f <- rir.compile(function(fc) {
  nfc <- length(fc)
  for(j in 2:nfc) {
  }
  nfc
})

f(c(1, 2))
f(c(1, 2))
f(c(1, 2))
stopifnot(f(c(1, 2, 3, 4)) == 4)
