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
