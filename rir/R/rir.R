# the following functions are intended for the API

rir.enableJit <- function(level=2, type="") {
    .Call("rir_jitEnable", type);
    if (level > 2)
        warning("level 3 is broken with rir");
    invisible(compiler:::enableJIT(level));
}
rir.disableJit <- function() {
    .Call("rir_jitDisable");
    invisible(compiler:::enableJIT(0));
}


rir.markOptimize <- function(what) {
    .Call("rir_markOptimize", what);
}

# Returns TRUE if the argument is a rir-compiled closure.
rir.isValidFunction <- function(what) {
    .Call("rir_isValidFunction", what);
}

# prints the disassembled rir function
rir.disassemble <- function(what) {
    invisible(.Call("rir_disassemble", what))
}

# compiles given closure, or expression and returns the compiled version.
rir.compile <- function(what) {
    .Call("rir_compile", what)
}

rir.eval <- function(what, env = globalEnv()) {
    .Call("rir_eval", what, env);
}

# these functions are for debugging purposes only and shoud not be used by normal users much

rir.cp <- function()  {
    .Call("rir_cp")
}

rir.src <- function()  {
    .Call("rir_src")
}

# returns the body of rir-compiled function. The body is the INTSXP vector containing its ast maps and code objects
rir.body <- function(f) {
    .Call("rir_body", f);
}

rir.da <- function(f) {
    .Call("rir_da", f)
}




rir.analysis.signature <- function(f) {
    x <- .Call("rir_analysis_signature", f)
    result = x[[1]]
    names(result) <- x[[2]]
    result
}
