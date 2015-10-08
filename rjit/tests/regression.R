require("rjit")

f <- jit.compile(
    function(x) {
        log <- "asdf"
        log <- strsplit(log, NULL)[[1L]]
        if("d" %in% log && any(ii <- x <= 0 & !is.na(x)))
            sum(ii)})

stopifnot(f(c(1,0,2,3,NA)) == 1)
