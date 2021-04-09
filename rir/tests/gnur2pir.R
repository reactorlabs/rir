test <- function(f, arg, expected) {
  f = compiler::cmpfun(f)
  compiler::disassemble(f)
  .Call('gnur2pir', f)
  stopifnot(identical(f(arg), expected))
}

test(function(a) 1,, 1)
test(function(a) a, 1, 1)
test(function(a) a+1, 1, 2)
