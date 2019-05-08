f <- pir.compile(rir.compile(function(a) a(b=1, 2)))
a <- function(a,b) c(a,b)
stopifnot(c(2,1) == pir.compile(rir.compile(function()f(a)))())

# forcing a promise can inspect the whole call stack
f <- pir.compile(rir.compile(function(x) sys.frame(x)))
g <- pir.compile(rir.compile(function(y) y))
h <- pir.compile(rir.compile(function() g(f(2))))
h()  # aborts if g's environment got elided

{
  f <- pir.compile(rir.compile(function(gpars) {
    numnotnull <- function(gparname) {
      match(gparname, names(gpars))
      gpars[[gparname]]
    }
    numnotnull('a')
    numnotnull('c')
  }))
  fc <- pir.compile(rir.compile(function() {
   f(list(a=1))
  }))
  fc()
}

{
  validGP <- pir.compile(rir.compile(function(gpars) {
    check.length <- function(gparname) {
        NULL
    }
    numnotnull <- function(gparname) {
      if (match(gparname, names(gpars))) {
          check.length(gparname)
      }
    }
    numnotnull('a')
  }))

  rir.compile(function() {
     validGP(list(a=1))
  })()
}

# speculative binop with deopt
rir.compile(function(){
    f <- rir.compile(function(a) a+2);
    f(1);
    f <- pir.compile(f);
    f(structure(1, class="foo"))
})()


# inlined frameStates:

if (Sys.getenv("PIR_DEOPT_CHAOS") != "1" &&
    Sys.getenv("PIR_WARMUP") != "2" &&
    Sys.getenv("RIR_SERIALIZE_CHAOS") == 0) {
    f <- pir.compile(rir.compile(function(x) g(x)))
    g <- rir.compile(function(x) h(x))
    h <- rir.compile(function(x) 1+i(x))
    i <- rir.compile(function(x) 40-x)
    
    stopifnot(f(-1) == 42)
    stopifnot(f(-1) == 42)
    
    hc1 = .Call("rir_invocation_count", h)
    ic1 = .Call("rir_invocation_count", i)
    g <- pir.compile(g)
    stopifnot(f(-1) == 42)
    
    ## Assert we are really inlined (ie. h and i are not called)
    hc2 = .Call("rir_invocation_count", h)
    ic2 = .Call("rir_invocation_count", i)
    stopifnot(hc1 == hc2)
    stopifnot(ic1 == ic2)
    
    ## Assert we deopt (ie. base version of h and i are invoked)
    stopifnot(f(structure(-1, class="asdf")) == 42)
    hc3 = .Call("rir_invocation_count", h)
    ic3 = .Call("rir_invocation_count", i)
    stopifnot(hc3 == hc2+1)
    stopifnot(ic3 == ic2+1)
}

# When subsequently calling the g inner function we must ensure
# that val is properly bind. This means that we must activate a
# the differnt SEXP everytime because val is bind to its enclosing
# environment (the environemnt of the current activation of f).
# This tests ensures that if an optimization tries to optimize
# this polymorphicness, the semantics are preserved
f <- function(val) {
    g <- function() val
    g()
}
h <- rir.compile(function(x) f(x))
stopifnot(h(1) == 1)
stopifnot(h(2) == 2)
h <- pir.compile(h)
stopifnot(h(3) == 3)


if (Sys.getenv("PIR_ENABLE") == "" && Sys.getenv("RIR_SERIALIZE_CHAOS") == 0) {
  require(compiler)
  old <- compiler::enableJIT(3)
  # test that we generate multiple versions
  p <- function(a=1,b=2) a+b
  for (i in 1:100) pir.compile(function() p())()
  for (i in 1:100) pir.compile(function() p(1))()
  for (i in 1:100) pir.compile(function() p(1,2))()
  stopifnot(length(.Call("rir_invocation_count", p)) > 3)
  compiler::enableJIT(old)
}

# scope analysis bug
f <- function() {a <- r(); b <- a; a[[1]] <- 2; a+b}
r <- function() 22
for (i in 1:5)
  stopifnot(f() == 24)
