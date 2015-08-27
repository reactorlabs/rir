if("rjit" %in% rownames(installed.packages())) {
    print("*****************")
    print("You have an existing version of rjit installed.")
    print("Please remove first (i.e. make clean) to avoid unexpected results!")
    exit()
}

dyn.load("librjit.so")

source("rjit/R/rjit.R")
library(compiler)

source("rjit/tests/stackless.R")
