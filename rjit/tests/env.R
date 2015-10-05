require("rjit")

.E <- new.env()

f1 <- jit.compile(function(){
  assign(".P", 123, envir = .E)
  get(".P", envir = .E)
})

f2 <- jit.compile(function(){
  .E <- new.env()
  assign(".P", 123, envir = .E)
  get(".P", .E)
})

stopifnot(f1() == 123)
stopifnot(f2() == 123)
