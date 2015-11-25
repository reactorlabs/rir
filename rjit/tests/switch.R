require("rjit")

t <- function(exp) {
    cexp = jit.compile(exp)
    a <- eval(exp)
    b <- eval(cexp)
    stopifnot(a == b)
}

tryCatch(eval(jit.compile(quote(switch()))), error=function(e){})
t(quote(switch("test")))
t(quote(switch(2)))
t(quote(switch("test", t=1, test=3, bla=5)))
t(quote(switch("test", t=1, test=, bla=5)))
t(quote(switch("test", t=1, test=)))
t(quote(switch("test", t=1, test=, bla=)))
t(quote(switch("test", t=1, bla=5, 123)))
t(quote(switch("test", t=1)))
t(quote(switch("test", t=1, tet=, bla=, i=3, test=45)))
