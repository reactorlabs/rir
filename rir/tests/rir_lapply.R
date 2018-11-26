rref <- bibentry(
    bibtype = "Manual",
    title = "R: A Language and Environment for Statistical Computing",
    author = person("R Core Team"),
    organization = "R Foundation for Statistical Computing",
    address = "Vienna, Austria",
    year = 2014,
    url = "http://www.R-project.org/")
compiler:::enableJIT(2)
(function() print(rref, style = "Bibtex"))()
compiler:::enableJIT(0)


a = bquote(a == a)
b = bquote(function(a=1)1)
c = bquote(function(a)1)
stopifnot(identical(rir.compile(function() bquote(a == a))(), a))
stopifnot(identical(rir.compile(function() bquote(function(a=1)1))(), b))
stopifnot(identical(rir.compile(function() bquote(function(a)1))(), c))

