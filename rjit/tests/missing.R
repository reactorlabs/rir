require("rjit")

f <- jit.compile(function(a, b=2) {
      b
  }
)
stopifnot(f(1,) == 2)

e <- quote(matrix(1,2,3)[1,])
ec <- jit.compile(e)
stopifnot(eval(e) == eval(ec))
