# ==============================
# === rir api:

rir.cp <- function()  {
    .Call("rir_cp")
}

rir.src <- function()  {
    .Call("rir_src")
}

rir.enableJit <- function() {
    .Call("rir_jitEnable");
    invisible(compiler:::enableJIT(1));
}
rir.disableJit <- function() {
    .Call("rir_jitDisable");
    invisible(compiler:::enableJIT(0));
}

rir.print <- function(what) {
    if (typeof(what) == "closure")
        what <- .Internal(bodyCode(what))
    invisible(.Call("rir_print", what))
}

rir.compile <- function(what) {
    if (typeof(what) == "closure") {
        .Call("rir_compileClosure", what)
    } else if (any(c("language", "symbol", "logical", "integer", "double", "complex", "character") == typeof(what))) {
        .Call("rir_compileAst", what, environment())
    } else {
       stop("Only bytecode expressions and asts can be jitted.")
    }
}

rir.exec <- function(what, env = globalenv()) {
    .Call("rir_exec", what, env)
}
