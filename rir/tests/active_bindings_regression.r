f = function() {
  makeActiveBinding("foo", identity, environment())
  rm(foo)
  TRUE
}
for (i in 1:10)
  stopifnot(tryCatch(f(), error=function(e) FALSE))
