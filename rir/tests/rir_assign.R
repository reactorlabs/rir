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

f14 <- rir.compile(function() {
  x <- matrix(5,nrow=4,ncol=4)
  x[2,3] <- 7
  
  stopifnot(x[[2,3]] == 7)
  stopifnot(x[[2,4]] == 5)
})
f14()

f15 <- rir.compile(function() {
  x <- matrix('foo',nrow=4,ncol=4)
  for (i in 1:4) {
    for (j in 1:4) {
      x[[i,j]] <- 'bar'
    }
  }
  
  stopifnot(x[[4,1]] == 'bar')
})
f15()

x <- matrix(5,nrow=4,ncol=4)
f16 <- rir.compile(function() {
  x <- matrix(5,nrow=4,ncol=4)
  x[2,3] <<- 7
})
f16()

stopifnot(x[2,3] == 7)
stopifnot(x[2,4] == 5)

f17 <- rir.compile(function() {
  x <- matrix('foo',nrow=5,ncol=5)
  for (i in 1:4) {
    for (j in 1:4) {
      x[[i,j]] <<- i * j
    }
  }
})
f17()

stopifnot(x[[3,4]] == '12')

f18 <- rir.compile(function() {
  x <- data.frame(a=c(1,2,3),b=c(4,5,6))
  x[, 2] <- x[, 2] + 1
  
  stopifnot(x[, 2] == c(5,6,7))
})
f18()

f19 <- rir.compile(function() {
  rzip <- function(gamma, theta=c(-2,.3)) {
    y <- gamma
    n <- length(y)
    lambda <- exp(gamma)
    eta <- theta[1] + exp(theta[2])*gamma
    p <- 1 - exp(-exp(eta))
    ind <- p > runif(n)
    y[!ind] <- 0
    np <- sum(ind)
    y[ind] <- qpois(runif(np, dpois(0, lambda[ind]), 1), lambda[ind])
    y
  }

  library(mgcv)
  set.seed(1)
  n <- 400
  dat <- gamSim(1, n=n)
  dat$y <- rzip(dat$f / 4 - 1)
  b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3), family=ziP(), data=dat)
})
f19()
