packageTests <- function() {
    testsHome = "../rjit/tests"
    for (file in list.files(testsHome)) {
        cat("    ")
        cat(file)
        cat("\n")
        source(paste(testsHome, file, sep="/"))
    }
    cat("IM IN UR TESTZ\n")
}

.First <- function() {

    cat("OH HAI! CAN I HAZ LIBRARIES\n")
    library("compiler")
    dyn.load("../build/librjit.so")
    cat("IM IN UR DLLZ\n")
    source("../rjit/R/rjit.R")
    cat("IM IN UR PACKAGE\n")
    packageTests()
    cat("AWSHUM\n")
    f <<- function(a, b) a - b
    x <<- jit.compile(function(a) {
        b <- 0;
        for (i in a) b <- b + i;
        b;
    })
}

.Last <- function() {
    cat("KTHXBYE!\n")
}
