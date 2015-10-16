require("tools")

packageTests <- function() {
    testsHome = "../rjit/tests"
    for (file in list.files(testsHome)) {
        if (file_ext(file) == "R") {
            cat("    ")
            cat(file)
            cat("\n")
            oldc = c
            source(paste(testsHome, file, sep="/"))
            c <<- oldc
        }
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
    #f <<- function(a, b)  a + b
    #fc <<- jit.compile(f)
    cat("AWSHUM\n")
#    test <<- function(x) {
#        .Call("jittest", x)
#    }
#    test(quote(a + b))
}

.Last <- function() {
    cat("KTHXBYE!\n")
}
