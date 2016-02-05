require(rjit)

g <- jit.compile(function(a, b, c, d, e, f) c)

f <- jit.compile(function() {
  stopifnot(g(1,2,3) == 3)
  stopifnot(g(2, 3, a=1, 4, d=5, 6) == 3)
  stopifnot(g(b=2, c=3, a=1, e=4, d=5, f=6) == 3)
  stopifnot(g(a=2, c=3, 1, 4, 5, 6) == 3)
})

f()
f()
f()
