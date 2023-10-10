quitEnv <- new.env(parent = globalenv())
quitEnv$quit <- function(...) {
    stop("quit called")
}
quitEnv$q <- quitEnv$quit

# Typically you want to use bin/tests instead, since that runs the tests in parallel.
# This is for when you want to run tests all in R, or want to debug in gdb/lldb.
for (f in sort(list.files("../rir/tests", pattern = "*.[rR]$", full.names = TRUE))) {
    print(paste("*** RUNNING ", basename(f)))
    tryCatch(source(f, echo=TRUE, local=quitEnv), error = function(e) {
        if (as.character(e) == "quit called") {
            print(paste("*** QUIT ", basename(f)))
        }
        print(paste("*** ERROR in ", basename(f)))
        print(e)
    })
}
