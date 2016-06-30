require("rjit")

f <- jit.compile(function(){
  if (1) 3
})
stopifnot(f() == 3)
stopifnot(f() == 3)


f <- jit.compile(function(){
  if (2L) 3
})
stopifnot(f() == 3)
stopifnot(f() == 3)

f <- jit.compile(function(){
  if (!1) 0 else 3
})
stopifnot(f() == 3)
stopifnot(f() == 3)


f <- jit.compile(function(){
  if (!2L) 1 else 3
})
stopifnot(f() == 3)
stopifnot(f() == 3)


f <- jit.compile(function(){
    lf <- length(1:2)
    if(lf) 1L:lf
})
stopifnot(length(f()) == 2)

seq.default <- jit.compile(seq.default)
stopifnot(length(seq(1:2)) == 2)
stopifnot(length(seq(1:2)) == 2)
stopifnot(length(seq(11:13)) == 3)
stopifnot(length(seq(seq(1:2))) == 2)
