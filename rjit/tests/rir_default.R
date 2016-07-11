f <- rir.compile(function(a=2) a)
stopifnot(f() == 2)

f <- rir.compile(function(a, b=a+2) b)
stopifnot(f(3) == 5)

g <- rir.compile(function(a, b=1) a+b)
f <- rir.compile(function(b = 123) g(b))
stopifnot(f() == 124)

f <- rir.compile(function(b = 123) typeof(b))
stopifnot(f() == "double")

f <- rir.compile(function(b = 123) cat(b))
stopifnot(f() == "123")
rir.compile(function() {
  stopifnot(f() == "123")
})()


f <- rir.compile(function(a = 1, b = 2) c(a, b))
stopifnot(f() == c(1,2))
stopifnot(f(2) == c(2,2))
stopifnot(f(,1) == c(1,1))
rir.compile(function() {
    stopifnot(f() == c(1,2))
    stopifnot(f(2) == c(2,2))
    stopifnot(f(,1) == c(1,1))
})()
