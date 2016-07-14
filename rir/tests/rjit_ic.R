require("rjit")

# function call
f <- function(a, b) a + b
fx <- jit.compile(function(x, y) f(x, y))
stopifnot(fx(1,2) == 3)

# function call with missing args
f <- function(a) missing(a)
fx <- jit.compile(function() f())
stopifnot(fx() == TRUE)

# native call ic
g <- jit.compile(function() 1)
f <- jit.compile(function() g())
stopifnot(f() == 1)
stopifnot(f() == 1)
stopifnot(f() == 1)

# native call ic args
g <- jit.compile(function(a) a)
f <- jit.compile(function(a) g(a))
stopifnot(f(1) == 1)
stopifnot(f(1) == 1)
stopifnot(f(1) == 1)

# native call ic args order
g <- jit.compile(function(a, b) a - b)
f <- jit.compile(function(a, b) g(a, b))
stopifnot(f(2,1) == 1)
stopifnot(f(2,1) == 1)
stopifnot(f(2,1) == 1)

# native call ic args order
g <- jit.compile(function(a, b) a - b)
f <- jit.compile(function(a) g(a, 1))
stopifnot(f(2) == 1)
stopifnot(f(2) == 1)
stopifnot(f(2) == 1)

# native call ic miss
g <- jit.compile(function() 1)
f <- jit.compile(function() g())
stopifnot(f() == 1)
stopifnot(f() == 1)
g <- jit.compile(function() 2)
stopifnot(f() == 2)
stopifnot(f() == 2)

f <- jit.compile(function(a,b,c,d) g(a,b) - g(c,b))
g <- jit.compile(function(a,b) a-b)
stopifnot(f(1,2,3,3) == f(1,2,3,3))
stopifnot(f(1,2,5,3) == f(1,2,5,3))
stopifnot(f(1,2,4,2) == f(1,2,4,3))
stopifnot(f(1,2,4) == f(1,2,4,3))
stopifnot(f(1,2,4,123) == f(1,2,4))
stopifnot(f(1,2,4,123) == f(1,2,4))
