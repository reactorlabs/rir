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
    #packageTests()
    cat("AWSHUM\n")
    f <<- function(a, b) a - b
    x <<- jit.compile(function(a) {
#        switch(a, 1,2,3,4,5,6,7,8,9,10)
         switch(a, a = 1, b = 2, c= 3, d = 4)
    })
}

.Last <- function() {
    cat("KTHXBYE!\n")
}
