f <- function(a,b,c) nargs()
g <- function() {
  f()
  f(1)
  f(1,2)
  f(1,2,3)
}
h <- function() g()

f <- pir.compile(rir.compile(f))
g <- pir.compile(rir.compile(g))
h <- pir.compile(rir.compile(h))

stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)




f <- function(a,b,c) nargs() + (if (!missing(a)) a else 1)
g <- function() {
  stopifnot(f() == 1)
  stopifnot(f(1) == 2)
  stopifnot(f(1,2) == 3)
  stopifnot(f(1,2,3) == 4)
  f(1,2)
}
h <- function() g()

f <- pir.compile(rir.compile(f))
g <- pir.compile(rir.compile(g))
h <- pir.compile(rir.compile(h))

stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)




f <- function(a,b,c) nargs() + a
g <- function(q) {
  stopifnot(f(q) == 2)
  print (f(q,2))
  stopifnot(f(q,2) == 3)
  stopifnot(f(q,2,3) == 4)
  f(q,2)
}
h <- function() g(1)

f <- pir.compile(rir.compile(f))
g <- pir.compile(rir.compile(g))
h <- rir.compile(h)

stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)
stopifnot(h()==3)

xx1 <- function() {
   ok = 0

   # returning a missing arg is supposed to error
   f <- function(a,b)
     a

   tryCatch(f(), error=function(e) ok <<- 1)
   stopifnot(ok == 1);
}

xx2 <- function() {
   ok = 0
   # forcing it too, the `(` function forces
   q <- function(a) (a)
   tryCatch(q(), error=function(e) ok <<- 1)
   stopifnot(ok == 1);
}

xx3 <- function() {
   # but passing on without forcing should not error
   h <- function(a) 1
   g <- function(a) h(a)
   g()
}

for (i in 1:10)
{xx1(); xx2(); xx3()}



f <- pir.compile(rir.compile(function(a,b,c) a))
g <- rir.compile(function() {
  f(1)
  f(1,2)
  f(1,2,3)
})

stopifnot(g()==1)
pir.compile(g)
stopifnot(g()==1)

