
jit.compile <- function(what, env = environment(what)) {
    if (typeof(what) == "closure") {
        bc = .Internal(bodyCode(what))
        native = .Call("jitAst", bc, formals(what), env)
        f = .Internal(bcClose(formals(what), native, env))
        attrs = attributes(what)
        if (!is.null(attrs))
            attributes(f) = attrs
        if (isS4(what))
            f = asS4(f)
        f
    } else if (any(c("language", "symbol", "logical", "integer", "double", "complex", "character") == typeof(what))) {
        .Call("jitAst", what, NULL, env)
    } else {
       stop("Only bytecode expressions and asts can be jitted.")
    }
}

jit.compileInPlace <- function(what, env = environment(what)) {
    #nat = .Internal(bodyCode(jit.compile(what, env)))
    nat = jit.compile(what, env)
    invisible(.Call("jitSwapForNative", what, nat))
}

jit.printTypefeedback <- function(what) {
    invisible(.Call("jitPrintTypefeedback", what))
}

jit.constants <- function(what) {
    if (typeof(what) == "closure")
        what = .Internal(bodyCode(what));
    .Call("jitConstants", what)
}

jit.llvm <- function(what) {
    if (typeof(what) == "closure")
        what = .Internal(bodyCode(what));
    invisible(.Call("jitLLVM", what))
}

# Creates a module with given name and compiles the functions listed in the second argument to it.
jit.compileFunctions <- function(moduleName, functions) {
    .Call("jitFunctions", moduleName, functions)
}

jit.compileEnvironment <- function(environment, moduleName ="rjit module") {
    fns = list()
    for (name in names(environment)) {
        x = environment[[name]]
        if (typeof(x) == "closure")
            fns[[name]] = x
    }
    if (length(fns) > 0)
        jit.compileFunctions(moduleName, as.pairlist(fns))
    invisible(NULL)
}

jit.enable <- function() .Call("jitEnable");
jit.disable <- function() .Call("jitDisable");

jit.setFlag <- function(flag, value) .Call("setFlag", flag, value)
