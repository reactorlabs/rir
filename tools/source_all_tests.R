# Typically you want to use bin/tests instead, since that runs the tests in parallel.
# This is for when you want to run tests all in R, or want to debug in gdb/lldb.
for (f in sort(list.files("../rir/tests", pattern = "*.[rR]$", full.names = TRUE))) {
    print(paste("*** RUNNING ", basename(f)))
    source(f)
}
