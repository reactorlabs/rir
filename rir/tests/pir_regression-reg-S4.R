options(useFancyQuotes=c)
require("Matrix") 
a <- c("", "", "tsp")
b <- function(d) {
    setClass("foo" )
}
e <- sapply(a, b)
str(e) # 
setOldClass(c("foo", "numeric"))
setClass("A", representation(slot1="numeric", slot2="logical"))
setClass("D1", "A" )
