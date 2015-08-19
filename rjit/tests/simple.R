require("compiler")
#require("rjit")

# simple expression evaluation
ex = jit(compile(quote(2 + 3)))
#stopifnot(eval(ex) == 5)

# simple expression evaluation with variables
a = 1 # suppress the warnings
b = 2
ex = jit(compile(quote(a + b)))
x = new.env()
x$a = 10
x$b = 12
#stopifnot(eval(ex, x) == 22)

# function evaluation
ex = jit(cmpfun(function(a, b) a + b))
#stopifnot(ex(3, 4) == 7)
