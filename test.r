h1 <- function(a) a + a;
h2 <- function(a) a + a;
g <- function(a) { h1(a); h1(a); h2(a); h2(a); }
h <- function(a) { g(1); g(c(11)); }
h()
h()
h()
h()
h()
h()
