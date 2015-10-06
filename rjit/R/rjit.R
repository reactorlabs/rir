
jit.compile <- function(what, env = environment(what)) {
    if (typeof(what) == "closure") {
        bc = .Internal(bodyCode(what))
        native = .Call("jitAst", bc)
        f = .Internal(bcClose(formals(what), native, env))
        attrs = attributes(what)
        if (!is.null(attrs))
            attributes(f) = attrs
        if (isS4(what))
            f = asS4(f)
        f
    } else if (typeof(what) %in% c("language", "symbol", "logical", "integer", "double", "complex", "character")) {
        .Call("jitAst", what)
    } else {
       stop("Only bytecode expressions and asts can be jitted.")
    }
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
