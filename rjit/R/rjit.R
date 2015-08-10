
jit.initialize <- function() {
    .Call("initializeRJIT")
}

jit <- function(what) {
    if (typeof(what) == "closure") {
        bc = .Internal(bodyCode(what))
        native = .Call("jit", bc)
        f = .Internal(bcClose(formals(what), native, environment(what)))
        attrs = attributes(what)
        if (!is.null(attrs))
            attributes(f) = attrs
        if (isS4(what))
            f = asS4(f)
        f
    } else if (typeof(what) == "bytecode") {
        .Call("jit", what)
   } else {
       stop("Only bytecode expressions and compiled functions can be jitted.")
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
