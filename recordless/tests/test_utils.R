#
# Test utilities for recordless optimization tests
#

# Path to the R binary currently running this test, so child processes use
# the same recordless-enabled build regardless of working directory.
.recordless_r_binary <- function() {
    file.path(R.home("bin"), "R")
}

# check(fn, ...patterns): compile `fn`, disassemble it, and assert every
# pattern (a fixed substring) appears somewhere in the output. Exits with a
# non-zero status if any pattern is missing.
#
# Runs in a CHILD process because rir.disassemble() writes via std::cout
# directly (rir/src/api.cpp:rirDisassemble), bypassing R's connection stack
# -- capture.output()/sink() cannot see it from within the same process. A
# child process's real stdout can be captured reliably via system2().
check <- function(fn, ...) {
    patterns <- list(...)

    fn_file <- tempfile(fileext = ".rds")
    on.exit(unlink(fn_file))
    saveRDS(fn, fn_file)

    script <- tempfile(fileext = ".R")
    on.exit(unlink(script), add = TRUE)
    writeLines(sprintf(
        'fn <- readRDS("%s"); rir.compile(fn); rir.disassemble(fn)', fn_file
    ), script)

    lines <- system2(.recordless_r_binary(),
                      args = c("--slave", "--no-init-file", "-f", script),
                      stdout = TRUE, stderr = TRUE)

    ok <- TRUE
    for (p in patterns) {
        found <- any(grepl(p, lines, fixed = TRUE))
        cat(if (found) "  PASS: " else "  FAIL: ", p, "\n", sep = "")
        if (!found)
            ok <- FALSE
    }
    if (!ok) {
        cat("\n--- disassembly ---\n")
        cat(paste(lines, collapse = "\n"), "\n")
        quit(status = 1, save = "no")
    }
    invisible(ok)
}
