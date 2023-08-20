####--- S4 Methods (and Classes)  --- see also ../src/library/methods/tests/

#### Instead of adding more tests depending on recommended packages,
#### re-facror into a separate script and treat like eval-etc-2.R

options(useFancyQuotes=FALSE)
require(methods)
assertError <- tools::assertError # "import"
##too fragile: showMethods(where = "package:methods")

## When this test comes too late, it failed too early in R <= 3.2.2
require(stats4)
detach("package:methods")
require("methods")
cc <- methods::getClassDef("standardGeneric")
cc ## (auto) print failed here, in R <= 3.2.2
stopifnot(.isMethodsDispatchOn()) ## was FALSE in R <= 3.2.2


## Needs cached primitive generic for '$'
new("envRefClass")# failed in R <= 3.2.0

##-- S4 classes with S3 slots [moved from ./reg-tests-1.R]
setClass("test1", representation(date="POSIXct"))
x <- new("test1", date=as.POSIXct("2003-10-09"))
stopifnot(format(x @ date) == "2003-10-09")
## line 2 failed in 1.8.0 because of an extraneous space in "%in%"

stopifnot(all.equal(3:3, 3.), all.equal(1., 1:1))

## trace (requiring methods):
f <- function(x, y) { c(x,y)}
xy <- 0
trace(f, quote(x <- c(1, x)), exit = quote(xy <<- x), print = FALSE)
fxy <- f(2,3)
stopifnot(identical(fxy, c(1,2,3)))
stopifnot(identical(xy, c(1,2)))
untrace(f)

## a generic and its methods

setGeneric("f")
setMethod("f", c("character", "character"), function(x,	 y) paste(x,y))

## trace the generic
trace("f", quote(x <- c("A", x)), exit = quote(xy <<- c(x, "Z")), print = FALSE)

## should work for any method

stopifnot(identical(f(4,5), c("A",4,5)),
          identical(xy, c("A", 4, "Z")))

stopifnot(identical(f("B", "C"), paste(c("A","B"), "C")),
          identical(xy, c("A", "B", "Z")))

## trace a method
trace("f", sig = c("character", "character"), quote(x <- c(x, "D")),
      exit = quote(xy <<- xyy <<- c(x, "W")), print = FALSE)

stopifnot(identical(f("B", "C"), paste(c("A","B","D"), "C")))
stopifnot(identical(xyy, c("A", "B", "D", "W")))
# got broken by Luke's lexical scoping fix:
#stopifnot(identical(xy, xyy))

## but the default method is unchanged
stopifnot(identical(f(4,5), c("A",4,5)),
          identical(xy, c("A", 4, "Z")))

removeGeneric("f")
## end of moved from trace.Rd


## print/show dispatch  [moved from  ./reg-tests-2.R ]
## The results  have waffled back and forth.
## Currently (R 2.4.0) the intent is that automatic printing of S4
## objects should correspond to a call to show(), as per the green
## book, p. 332.  Therefore, the show() method is called, once defined,
## for auto-printing foo, regardless of the S3 or S4 print() method.
## (But most of this example is irrelevant if one avoids S3 methods for
## S4 classes, as one should.)
setClass("bar", representation(a="numeric"))
foo <- new("bar", a=pi)
foo
show(foo)
print(foo)

setMethod("show", "bar", function(object){cat("show method\n")})
show(foo)
foo
print(foo)
# suppressed because output depends on current choice of S4 type or
# not.  Can reinstate when S4 type is obligatory
# print(foo, digits = 4)

## DON'T DO THIS:  S3 methods for S4 classes are a design error JMC iii.9.09
## print.bar <- function(x, ...) cat("print method\n")
## foo
## print(foo)
## show(foo)

setMethod("print", "bar", function(x, ...){cat("S4 print method\n")})
foo
print(foo)
show(foo)
## calling print() with more than one argument suppresses the show()
## method, largely to prevent an infinite loop if there is in fact no
## show() method for this class.  A better solution would be desirable.
print(foo, digits = 4)

cn <- "integer or NULL"
setClassUnion(cn, members = c("integer", "NULL"))
setClass("c1", representation(x = "integer", code = cn))
stopifnot(exprs = {
    cn %in% extends(getClass("NULL"))
    cn %in% extends(getClass(".NULL"))
    cn %in% extends(getClass("integer"))
})
nc <- new("c1", x = 1:2)
str(nc)# gave ^ANULL^A in 2.0.0
##

showMethods("coerce", classes=c("matrix", "numeric"))
## {gave wrong result for a while in R 2.4.0}

## Most for "mle" in stats4:
for(f in c("coef", "confint", "logLik", "plot", "profile",
	   "show", "summary", "update", "vcov"))
    if(!hasMethods(f)) stop("no S4 methods found for ", f)


##--- "[" fiasco before R 2.2.0 :
d2 <- data.frame(b= I(matrix(1:6,3,2)))
## all is well:
d2[2,]
stopifnot(identical(d2[-1,], d2[2:3,]))
## Now make "[" into S4 generic by defining a trivial method
setClass("Mat", representation(Dim = "integer", "VIRTUAL"))
setMethod("[", signature(x = "Mat",
			 i = "missing", j = "missing", drop = "ANY"),
	  function (x, i, j, drop) x)
## Can even remove the method: it doesn't help
removeMethod("[", signature(x = "Mat",
                            i = "missing", j = "missing", drop = "ANY"))
d2[1:2,] ## used to fail badly; now okay
stopifnot(identical(d2[-1,], d2[2:3,]))
## failed in R <= 2.1.x


## Fritz' S4 "odditiy"
setClass("X", representation(bar="numeric"))
setClass("Y", contains="X")
## Now we define a generic foo() and two different methods for "X" and
## "Y" objects for arg missing:
setGeneric("foo", function(object, arg) standardGeneric("foo"))
setMethod("foo", signature(object= "X", arg="missing"),
          function(object, arg) cat("an X object with bar =", object@bar, "\n"))
setMethod("foo", signature(object= "Y", arg="missing"),
          function(object, arg) cat("a Y object with bar =", object@bar, "\n"))
## Finally we create a method where arg is "logical" only for class
## "X", hence class "Y" should inherit that:
setMethod("foo", signature(object= "X", arg= "logical"),
          function(object, arg) cat("Hello World!\n") )
## now create objects and call methods:
y <- new("Y", bar=2)
## showMethods("foo")
foo(y)
foo(y, arg=TRUE)## Hello World!
## OK, inheritance worked, and we have
## showMethods("foo")
foo(y)
## still 'Y' -- was 'X object' in R < 2.3


## Multiple inheritance
setClass("A", representation(x = "numeric"))
setClass("B", representation(y = "character"))
setClass("C", contains = c("A", "B"), representation(z = "logical"))
new("C")
setClass("C", contains = c("A", "B"), representation(z = "logical"),
         prototype = prototype(x = 1.5, y = "test", z = TRUE))
(cc <- new("C"))
## failed reconcilePropertiesAndPrototype(..) after svn r37018
stopifnot(identical(selectSuperClasses("C", dropVirtual = TRUE), c("A", "B")),
	  0 == length(.selectSuperClasses(getClass("B")@contains)))

## "Logic" group -- was missing in R <= 2.4.0
stopifnot(all(getGroupMembers("Logic") %in% c("&", "|")),
	  any(getGroupMembers("Ops") == "Logic"))
setClass("brob", contains="numeric")
b <- new("brob", 3.14)
logic.brob.error <- function(nm)
    stop("logic operator '", nm, "' not applicable to brobs")
logic2 <- function(e1,e2) logic.brob.error(.Generic)
setMethod("Logic", signature("brob", "ANY"), logic2)
setMethod("Logic", signature("ANY", "brob"), logic2)
## Now ensure that using group members gives error:
assertError(b & b)
assertError(b | 1)
assertError(TRUE & b)


## methods' hidden cbind() / rbind:
setClass("myMat", representation(x = "numeric"))
setMethod("cbind2", signature(x = "myMat", y = "missing"), function(x,y) x)
m <- new("myMat", x = c(1, pi))
stopifnot(identical(m, methods:::cbind(m)), identical(m, cbind(m)))


## explicit print or show on a basic class with an S4 bit
## caused infinite recursion
setClass("Foo", representation(name="character"), contains="matrix")
(f <- new("Foo", name="Sam", matrix()))
f2 <- new("Foo", .Data = diag(2), name="Diag")# explicit .Data
(m <- as(f, "matrix"))
## this has no longer (2.7.0) an S4 bit: set it explicitly just for testing:
stopifnot(isS4(m. <- asS4(m)),
          identical(m, f@.Data),
	  .hasSlot(f, "name"))# failed in R <= 2.13.1
show(m.)
print(m.)
## fixed in 2.5.0 patched

## callGeneric inside a method with new arguments {hence using .local()}:
setGeneric("Gfun", function(x, ...) standardGeneric("Gfun"),
	   useAsDefault = function(x, ...) sum(x, ...))
setClass("myMat", contains="matrix")
setClass("mmat2", contains="matrix")
setClass("mmat3", contains="mmat2")
setMethod(Gfun, signature(x = "myMat"),
	  function(x, extrarg = TRUE) {
	      cat("in 'myMat' method for 'Gfun() : extrarg=", extrarg, "\n")
	      Gfun(unclass(x))
	  })
setMethod(Gfun, signature(x = "mmat2"),
	  function(x, extrarg = TRUE) {
	      cat("in 'mmat2' method for 'Gfun() : extrarg=", extrarg, "\n")
	      x <- unclass(x)
	      callGeneric()
	  })
setMethod(Gfun, signature(x = "mmat3"),
	  function(x, extrarg = TRUE) {
	      cat("in 'mmat3' method for 'Gfun() : extrarg=", extrarg, "\n")
	      x <- as(x, "mmat2")
	      callGeneric()
	  })
wrapG <- function(x, a1, a2) {
    myextra <- missing(a1) && missing(a2)
    Gfun(x, extrarg = myextra)
}

(mm <- new("myMat", diag(3)))
Gfun(mm)
stopifnot(identical(wrapG(mm),    Gfun(mm, TRUE)),
          identical(wrapG(mm,,2), Gfun(mm, FALSE)))

Gfun(mm, extrarg = FALSE)
m2 <- new("mmat2", diag(3))
Gfun(m2)
Gfun(m2, extrarg = FALSE)
## The last two gave Error ...... variable ".local" was not found
(m3 <- new("mmat3", diag(3)))
Gfun(m3)
