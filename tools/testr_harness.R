compare_results <- function (a, b) 
{
    if (is.language(a)) 
        a <- as.expression(a)
    if (identical(all.equal(a, b), TRUE) || identical(as.expression(a), 
        as.expression(b))) {
        return(TRUE)
    }
    else if (identical(all.equal(is.na(a), is.na(b)), TRUE)) {
        if ((typeof(a) %in% c("builtin", "special", "closure", 
            "symbol")) || (typeof(b) %in% c("builtin", "special", 
            "closure", "symbol"))) 
            return(FALSE)
        aa <- a[!is.na(a)]
        bb <- b[!is.na(b)]
        return((length(aa) == 0) && (length(bb) == 0)) || identical(all.equal(aa, 
            bb), TRUE)
    }
    else {
        return(FALSE)
    }
}

test <- function(id, code, o, e, w) {
    compiled_code <- jit.compile(substitute(code))
    if (!doTest(id, compiled_code, o, e, w)) {
        if (!doTest(id, code, o, e, w)) {
            print("test is broken");
            quit(3)
        } else {
            print("test failed");
            quit(1)
        }
    }
}

doTest <- function(id, code, o, e, w) {
    res <- tryCatch({
        eval(code, envir=new.env())
    }, error = function(e) e$message)

    success <- FALSE

    print(res)

    # TODO check warnings
    if (!missing(e)) {
        e == res
    } else {
        compare_results(o, res)
    }
}

