# Sometimes this is actually different, but won't break tests
# WARNING: If RIR_WARMUP chanfes in the future, this might need to change too; fes in the future, this might need to change too; fes in the future, this might need to change too
f <- rir.compile(function(a, b, c) {
  a + b
})
x <- 5
y <- 2

f(x, y, x); f(x, y, x); f(x, y, x)
f(x, y, 4); f(x, y, 4); f(x, y, 4)
f(x, y, 4L); f(x, y, 4L); f(x, y, 4L)
f(x, 4, y); f(x, 4, y); f(x, 4, y)
f(x, 4L, y); f(x, 4L, y); f(x, 4L, y)
f(4, x, y); f(4, x, y); f(4, x, y)
f(4L, x, y); f(4L, x, y); f(4L, x, y)
f(x, 4, 4); f(x, 4, 4); f(x, 4, 4)
f(x, 4, 4L); f(x, 4, 4L); f(x, 4, 4L)
f(x, 4L, 4); f(x, 4L, 4); f(x, 4L, 4)
f(x, 4L, 4L); f(x, 4L, 4L); f(x, 4L, 4L)
f(4, x, 4); f(4, x, 4); f(4, x, 4)
f(4, x, 4L); f(4, x, 4L); f(4, x, 4L)
f(4L, x, 4); f(4L, x, 4); f(4L, x, 4)
f(4L, x, 4L); f(4L, x, 4L); f(4L, x, 4L)
f(4, 4, x); f(4, 4, x); f(4, 4, x)
f(4, 4L, x); f(4, 4L, x); f(4, 4L, x)
f(4L, 4, x); f(4L, 4, x); f(4L, 4, x)
f(4L, 4L, x); f(4L, 4L, x); f(4L, 4L, x)
f(4, 4, 4); f(4, 4, 4); f(4, 4, 4)
f(4, 4, 4L); f(4, 4, 4L); f(4, 4, 4L)
f(4, 4L, 4); f(4, 4L, 4); f(4, 4L, 4)
f(4, 4L, 4L); f(4, 4L, 4L); f(4, 4L, 4L)
f(4L, 4, 4); f(4L, 4, 4); f(4L, 4, 4)
f(4L, 4L, 4); f(4L, 4L, 4); f(4L, 4L, 4)
f(4L, 4, 4L); f(4L, 4, 4L); f(4L, 4, 4L)
f(4L, 4L, 4L); f(4L, 4L, 4L); f(4L, 4L, 4L)
