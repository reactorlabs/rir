if("rjit" %in% rownames(installed.packages())) {
    print("*****************")
    print("You have an existing version of rjit installed.")
    print("Please remove first (i.e. make clean) to avoid unexpected results!")
    exit()
}

if (Sys.info()["sysname"] == "Darwin") {
    dyn.load("librjit.dylib")
} else {
    dyn.load("librjit.so")
}

source("rjit/R/rjit.R", verbose = TRUE)
library(compiler)

source("rjit/tests/stackless.R", verbose = TRUE)
