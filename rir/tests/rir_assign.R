a <- function(x) x
"a<-" <- function(x, value) value

f2 <- rir.compile(function () {
    x <- 1
    a(x) <- 2
    x
})

f3 <- rir.compile(function () {
    x <- 1
    a(a(x)) <- 3
    x
})

f4 <- rir.compile(function () {
    x <- 1
    a(a(a(x))) <- 4
    x
})

stopifnot(f2() == 2);
stopifnot(f3() == 3);
stopifnot(f4() == 4);

b <- function(x, y) x[[y]]
"b<-" <- function(x, y, value) list(x[0:(y-1)], value, x[(y+1:-1)])

f5 <- rir.compile(function() {
  x <- list(1,2,3)
  b(x, 2) <- 5
  b(x, 2)
})
f6 <- rir.compile(function() {
  x <- list(1,list(2,3,4),5)
  stopifnot(b(b(x, 2), 2) == 3)
  b(b(x, 2), 2) <- 6
  b(b(x, 2), 2)
})
f7 <- rir.compile(function() {
  x <- list(1,list(2,list(3,4,5),6),-1)
  stopifnot(b(b(b(x, 2), 2), 2) == 4)
  b(b(b(x, 2), 2), 2) <- 7
  b(b(b(x, 2), 2), 2)
})

stopifnot(f5() == 5);
stopifnot(f6() == 6);
stopifnot(f7() == 7);

f8 <- rir.compile(function() {
  x <- list(1,list(2,list(3,4,5),6),-1)
  h <- function() 2

  stopifnot(b(b(b(x, 2), 2), 2) == 4)
  b(b(b(x, h()), h()), h()) <- 8
  b(b(b(x, 2), 2), 2)
})

stopifnot(f8() == 8);

f9 <- rir.compile(function() {
  x <- c(1,2,3)
  x[2] <- 123
  x[2]
})

stopifnot(f9() == 123)

f10 <- rir.compile(function() {
  x <- 123
  x[1] <- 55
})

stopifnot(f10() == 55)

f11 <- rir.compile(function() {
  a <- c(1,2)
  f <- function() {
    a[[1]] <- 3
    a[[1]]
  }
  rir.disassemble(f)
  stopifnot(f() == 3)
  a[[1]]
})

stopifnot(f11() == 1)

f12 <- rir.compile(function() {
  a <- c(1,2)
  f <- function() {
    a[1] <- 3
    a[1]
  }
  stopifnot(f() == 3)
  a[1]
})

stopifnot(f12() == 1)

x <- c(NA, 1, 2)
f13 <- rir.compile(function() {
    x <- c(1, 2, NA)
    # is.na(x) should use the local x!
    # see https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Subset-assignment
    x[is.na(x)] <<- 0
})
f13()

stopifnot(x[[3]] == 0)
stopifnot(any(is.na(x)))
