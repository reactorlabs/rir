#!/usr/bin/env Rscript
#
# NoRecord: a second read of the same variable, with no intervening write,
# depends on the first read's recorded type instead of recording again.
#
source("recordless/tests/test_utils.R")

h <- function(a) { a; a }

check(h,
    "Type#0",
    "NoRecord Type#1 (dep: #0)"
)
