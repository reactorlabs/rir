setClass("a", slots=c(myint="numeric"))

myfun.a <- function(x) {
  print(.Generic)
}

myfun <- function(x) {
  # dispatch on the class of x
  UseMethod("myfun")
}

x <- new("a")
myfun(x)
myfun(x)
myfun(x)
