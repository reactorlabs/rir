eagerInnerFun = function(b) {
  f = function(a) {
    if (a)
      1
    eval("dont inline")
  }
  f(b)
}
a=1
stopifnot(
  pir.check(eagerInnerFun, EagerCallArgs, warmup=function(f) {f(a)})
)

eagerInnerFun = function(b) {
  f = function(a) {
    if (a)
      1
    eval("dont inline")
  }
  # potentially reflective promise cannot be eagerly forced
  f(b())
}
a=function() {eval("1");TRUE}
stopifnot(
  pir.check(eagerInnerFun, LazyCallArgs, warmup=function(f) {f(a)})
)
