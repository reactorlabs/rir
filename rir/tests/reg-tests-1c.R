## merge.dendrogram(), PR#15648
mkDend <- function(n, lab, method = "complete",
                   ## gives *ties* often:
		   rGen = function(n) 1+round(16*abs(rnorm(n)))) {
    stopifnot(is.numeric(n), length(n) == 1, n >= 1, is.character(lab))
    a <- matrix(rGen(n*n), n, n)
    colnames(a) <- rownames(a) <- paste0(lab, 1:n)
    .HC. <<- hclust(as.dist(a + t(a)), method=method)
    as.dendrogram(.HC.)
}

## recursive dendrogram methods and deeply nested dendrograms
op <- options(expressions = 999)# , verbose = 2) # -> max. depth= 961
set.seed(11); d <- mkDend(1500, "A", method="single")
rd <- reorder(d, nobs(d):1)
## Error: evaluation nested too deeply: infinite recursion .. in R <= 3.2.3
stopifnot(is.leaf(r1 <- rd[[1]]),    is.leaf(r2 <- rd[[2:1]]),
	  attr(r1, "label") == "A1458", attr(r2, "label") == "A1317")
options(op)# revert

## recursive dendrogram methods and deeply nested dendrograms
op <- options(expressions = 999)# , verbose = 2) # -> max. depth= 961
set.seed(11); d <- mkDend(1500, "A", method="single")
print(d[[1]])
rd <- reorder(d, nobs(d):1)
print(rd[[1]])
## Error: evaluation nested too deeply: infinite recursion .. in R <= 3.2.3
stopifnot(is.leaf(r1 <- rd[[1]]),    is.leaf(r2 <- rd[[2:1]]),
	  attr(r1, "label") == "A1458", attr(r2, "label") == "A1317")
options(op)# revert

## recursive dendrogram methods and deeply nested dendrograms
op <- options(expressions = 999)# , verbose = 2) # -> max. depth= 961
set.seed(11); d <- mkDend(1500, "A", method="single")
print(d[[1]])
rd <- reorder(d, nobs(d):1)
print(rd[[1]])
## Error: evaluation nested too deeply: infinite recursion .. in R <= 3.2.3
stopifnot(is.leaf(r1 <- rd[[1]]),    is.leaf(r2 <- rd[[2:1]]),
	  attr(r1, "label") == "A1458", attr(r2, "label") == "A1317")
options(op)# revert

## recursive dendrogram methods and deeply nested dendrograms
op <- options(expressions = 999)# , verbose = 2) # -> max. depth= 961
set.seed(11); d <- mkDend(1500, "A", method="single")
print(d[[1]])
rd <- reorder(d, nobs(d):1)
print(rd[[1]])
## Error: evaluation nested too deeply: infinite recursion .. in R <= 3.2.3
stopifnot(is.leaf(r1 <- rd[[1]]),    is.leaf(r2 <- rd[[2:1]]),
	  attr(r1, "label") == "A1458", attr(r2, "label") == "A1317")
options(op)# revert
