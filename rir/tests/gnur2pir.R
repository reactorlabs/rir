test <- function(f, arg, expected) {
  f = compiler::cmpfun(f)
  compiler::disassemble(f)
  .Call('gnur2pir', f)
  stopifnot(identical(f(arg), expected))
}

test(function(a) 1,, 1)
test(function(a) a, 1, 1)
test(function(a) a+1, 1, 2)
test(function(a) if (a) 1, TRUE, 1)
test(function(a) if (a) 1, FALSE, NULL)
test(function(a) 1 + if (a) 1, T, 2)
test(function(a) 2 - 1 + if (a) 1, T, 2)
