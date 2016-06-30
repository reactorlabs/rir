# ==============================
# === rir api:

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

# ==============================
# === rjit api:

rjit.enableJit <- function() {
    .Call("rjit_jitEnable");
    invisible(compiler:::enableJIT(1));
}
rjit.disableJit <- function() {
    .Call("rjit_jitDisable");
    invisible(compiler:::enableJIT(0));
}

rjit.print <- function(what) {
    if (typeof(what) == "closure")
        what = .Internal(bodyCode(what));
    invisible(.Call("rjit_print", what))
}

# DEPRECATED
jit.compile <- function(what, env = environment(what)) {
    rjit.compile(what, env);
}

rjit.compile <- function(what, env = environment(what)) {
    if (typeof(what) == "closure") {
        bc = .Internal(bodyCode(what))
        native = .Call("rjit_jit", bc, formals(what), env)
        f = .Internal(bcClose(formals(what), native, env))
        attrs = attributes(what)
        if (!is.null(attrs))
            attributes(f) = attrs
        if (isS4(what))
            f = asS4(f)
        f
    } else if (any(c("language", "symbol", "logical", "integer", "double", "complex", "character") == typeof(what))) {
        .Call("rjit_jit", what, NULL, env)
    } else {
       stop("Only bytecode expressions and asts can be jitted.")
    }
}

rjit.compileInPlace <- function(what, env = environment(what)) {
    #nat = .Internal(bodyCode(jit.compile(what, env)))
    nat = rjit.compile(what, env)
    invisible(.Call("rjit_SwapForNative", what, nat))
}

# rjit internal api

rjit.internal.printTypefeedback <- function(what) {
    invisible(.Call("rjit_PrintTypefeedback", what))
}

rjit.internal.getConstants <- function(what) {
    if (typeof(what) == "closure")
        what = .Internal(bodyCode(what));
    .Call("rjit_GetConstants", what)
}

rjit.internal.setFlag <- function(flag, value) .Call("rjit_setFlag", flag, value)
