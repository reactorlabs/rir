require("compiler")
require("rjit")

jit.initialize()

# does not work atm

# # simple expression evaluation
# ex = jit(compile(quote(2 + 3)))
# stopifnot(eval(ex) == 5)
# 
# # simple expression evaluation with variables
# ex = jit(compile(quote(a + b)))
# x = new.env()
# x$a = 10
# x$b = 12
# stopifnot(eval(ex, x) == 22)

# function evaluation
stopifnot(jit(cmpfun(function() 2+4))() == 6)

ex = jit(cmpfun(function(a, b) a + b))
stopifnot(ex(3, 4) == 7)
