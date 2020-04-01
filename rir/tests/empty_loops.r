f = function(a) {
  while(TRUE) {}
}
pir.compile(rir.compile(f))
f = function(a) {
  while(FALSE) {}
}
pir.compile(rir.compile(f))
f <- function() {
  qq <- 3 > 2
  while (qq) {
  }
}
pir.compile(rir.compile(f))
