f <- rir.compile(function() {
  x <- 1
  for (i in 1L:x) NULL
})
f()
f()
f()
