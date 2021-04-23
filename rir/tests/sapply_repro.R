a <- c( "", "", "",  "formals(lm)[[2]]")
b <- sapply(a, function(d) eval(parse(text = d)))
print(b)
t(sapply(b, function(d) c(typeof(d))))
