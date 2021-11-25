f <- function() {
    x <- c("a", "b")
    names(x) <- x
}
x <- f()
x <- f()
x <- f()
x <- f()
y <- 1:5
y[x]
print(x)
print(attributes(x))


f <- function() {
    y <- data.frame(a = 1:5)
    x <- c("a", "b")
    names(x) <- x
    y[, x] <- 0
    y
}
f()
f()
f()
f()

y <- data.frame(a = 1:2)
f <- function() {
    x <- c("a", "b")
    names(x) <- x
    y[, x] <- 0
    y
}
f()
f()
f()
f()
