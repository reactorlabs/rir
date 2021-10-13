f <- function() quote(...)
for (i in 1:5)
    stopifnot(identical(f(), quote(...)))
