if (Sys.getenv("PIR_NATIVE_BACKEND") != "1") {
# libjit backend is broken...

g = function() {
  f = function(a,b) {
    r = a %% b
    print(paste(a,b,r))
    r
  }
  stopifnot(f(1L,2L) == 1L)
  stopifnot(f(-1L,2L) == 1L)
  stopifnot(f(1L,-2L) == -1L)
  stopifnot(f(-1L,-2L) == -1L)
  stopifnot(f(1L,2L) == 1L)
  stopifnot(f(3L,-2L) == -1L)
  stopifnot(f(0L,-2L) == 0L)
  stopifnot(is.na(f(0L,-0L)))
  stopifnot(is.na(f(0L,0L)))
}

for (i in 1:10)
  g()

g = function() {
  f = function(a,b) {
    r = a %% b
    print(paste(a,b,r))
    r
  }
  stopifnot(f(1,2) == 1)
  stopifnot(f(-1,2) == 1)
  stopifnot(f(1,-2) == -1)
  stopifnot(f(-1,-2) == -1)
  stopifnot(f(1,2) == 1)
  stopifnot(f(3,-2) == -1)
  stopifnot(f(0,-2) == 0)
  stopifnot(is.na(f(0,-0)))
  stopifnot(is.na(f(0,0)))
}

for (i in 1:10)
  g()

}
