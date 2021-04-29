a <- c( "", "", "",  "formals(lm)[[2]]")
b <- sapply(a, function(d) eval(parse(text = d)))
print(b)
t(sapply(b, function(d) c(typeof(d))))

cex3 <- c("NULL", "1", "1:1", "1i", "list(1)", "data.frame(x = 1)",
   "pairlist(pi)", "c", "lm", "formals(lm)[[1]]",  "formals(lm)[[2]]",
   "y ~ x","expression((1))[[1]]", "(y ~ x)[[1]]",
   "expression(x <- pi)[[1]][[1]]")
lex3 <- sapply(cex3, function(x) eval(parse(text = x)))
mex3 <- t(sapply(lex3,
                  function(x) c(typeof(x), storage.mode(x), mode(x))))
