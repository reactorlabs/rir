# the following functions are intended for the API

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

# prints the disassembled versions of list of rir compiled expressions
rir.disassemble.file <- function(code_objects) {
  Map(rir.disassemble, code_objects)
  NULL
}

# compiles given closure, or expression and returns the compiled version.
rir.compile <- function(what) {
    # TODO: gnu-r compile function takes optional environment argument for expressions
    # this just uses globalenv for now
    if (typeof(what) == "closure")
        .Call("rir_compile", what, environment(what))
    else
        .Call("rir_compile", what, globalenv())
}

# compiles code of the given file and returns the list of compiled version.
rir.compile.file <- function(file) {
  expr <- parse(file = file)
  Map(rir.compile, as.list(expr))
}

rir.eval <- function(what, env = globalenv()) {
    .Call("rir_eval", what, env);
}

# returns the body of rir-compiled function. The body is the vector containing its ast maps and code objects
rir.body <- function(f) {
    .Call("rir_body", f);
}

rir.analysis.signature <- function(f) {
    x <- .Call("rir_analysis_signature", f)
    result = x[[1]]
    names(result) <- x[[2]]
    result
}
