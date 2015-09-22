require("compiler")
require("rjit")

# simple constant
ex <- jit.compile(quote(1))
stopifnot(eval(ex) == 1)

# simple variable
ex <- jit.compile(quote(a))
a <- 10
stopifnot(eval(ex) == 10)
a <- 45
stopifnot(eval(ex) == 45)

# simple function call - builtin consts
ex <- jit.compile(quote(1 + 2))
stopifnot(eval(ex) == 3)

# simple function call - builtin vars
ex <- jit.compile(quote(a + b))
a <- 20
b <- 10
stopifnot(eval(ex) == 30)

# simple function call - user function
f <- function(a, b) a + b
ex <- jit.compile(quote(f(3, 4)))
stopifnot(eval(ex) == 7)
# call again to test ic
stopifnot(eval(ex) == 7)

# blocks
ex <- jit.compile(quote({ 1 ; 2; 3; }))
stopifnot(eval(ex) == 3)
stopifnot(eval(ex) == 3)

# creating a function compiles it
ex <- jit.compile(quote(function(a, b) a + b))
f = eval(ex)
stopifnot(typeof(.Internal(bodyCode(f))) == "native")
stopifnot(typeof(.Internal(bodyCode(f))) == "native")

# created function can be evaluated
stopifnot(f(1, 2) == 3)
stopifnot(f(1, 2) == 3)

# a function can be compiled
ex <- jit.compile(function(a, b) a + b)
stopifnot(typeof(.Internal(bodyCode(ex))) == "native")
stopifnot(ex(10, -2) == 8)

# return statement works
f <- jit.compile(function(a, b) { return(1); 2 })
stopifnot(f(1, 2) == 1)
stopifnot(f(1, 2) == 1)

# empty return returns NULL
f <- jit.compile(function(a, b) return())
stopifnot(f() == NULL)
stopifnot(f() == NULL)

# return in a promise
f <- function(a) {
    if (a) ptest <<- 10;
    100;
}
fx <- jit.compile(function(a) {
    f(if(a) TRUE else return(66))
})
stopifnot(fx(0) == 66)
stopifnot(fx(0) == 66)
stopifnot(! exists("ptest"))
stopifnot(! exists("ptest"))
stopifnot(fx(1) == 100)
stopifnot(fx(1) == 100)
stopifnot(exists("ptest"))
stopifnot(exists("ptest"))
stopifnot(ptest == 10)
stopifnot(ptest == 10)

# condition
fx <- jit.compile(function(a) if (a) 1 else 2)
stopifnot(fx(10) == 1)
stopifnot(fx(0) == 2);

# condition, no else
fx <- jit.compile(function(a) if (a) 1)
stopifnot(fx(10) == 1)
stopifnot(fx(0) == NULL);

# repeat loop w/o context
fx <- jit.compile(function(a) {
    b = 0
    repeat {
      a = a - 1
      if (a == 0)
          break
      if (a == 2)
          next
      b = b + a
    }
    b
})
stopifnot(fx(4) == 4)

# while loop w/o context
fx <- jit.compile(function(a) {
    b = 0
    while (a > 0) {
      a = a - 1
      if (a == 2)
          next
      b = b + a
    }
    b
})
stopifnot(fx(4) == 4)

# for loop w/o context

fx <- jit.compile(function(a) {
    b = 0
    for (i in a) {
        if (i == 3)
            break
        if (i == 7)
            next
        b = b + i
    }
    b
})
stopifnot(fx(c(3,2,1)) == 0);
stopifnot(fx(c(2, 7, 1)) == 3);

#integral switch
fx <- jit.compile(function(a) {
    switch(a, 1,2,3,4,5,6)
    })
stopifnot(fx(1) == 1)
stopifnot(fx(10) == 6)
stopifnot(fx(0) == 6)
stopifnot(fx(3) == 3)

#character switch
fx <- jit.compile(function(a) {
    switch(a, a = 1, b = 2, c = 3, d = 4)
    })
stopifnot(fx("a") == 1)
stopifnot(fx("b") == 2)
stopifnot(fx("c") == 3)
stopifnot(fx("d") == 4)
stopifnot(fx("df") == 4)
stopifnot(fx(1) == 1)
stopifnot(fx(2) == 2)
stopifnot(fx(3) == 3)
stopifnot(fx(4) == 4)
stopifnot(fx(5) == 4)

#character switch default value not last
fx <- jit.compile(function(a) {
    switch(a, a = 1, b = 2, 10, c = 3, d = 4)
    })
stopifnot(fx("a") == 1)
stopifnot(fx("b") == 2)
stopifnot(fx("c") == 3)
stopifnot(fx("d") == 4)
stopifnot(fx("df") == 10)
stopifnot(fx(1) == 1)
stopifnot(fx(2) == 2)
stopifnot(fx(3) == 10)
stopifnot(fx(4) == 3)
stopifnot(fx(5) == 4)
stopifnot(fx(6) == 4)

# compiling more functions
f1 <- function(a, b) { a + b }
f2 <- function(c, d) { f1(c, d) }
jit.compileFunctions("testModule", as.pairlist(list(f1 = f1, f2 = f2)))
stopifnot(typeof(.Internal(bodyCode(f1))) == "native")
stopifnot(typeof(.Internal(bodyCode(f2))) == "native")
stopifnot(f2(1,2) == 3)

# compiling environments
env = new.env()
env$f1 <- function(a, b) { a + b }
env$f2 <- function(c, d) { f1(c, d) }
jit.compileEnvironment(env)
stopifnot(typeof(.Internal(bodyCode(env$f1))) == "native")
stopifnot(typeof(.Internal(bodyCode(env$f2))) == "native")
