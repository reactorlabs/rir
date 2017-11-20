# the following functions are intended for the API

rir.markOptimize <- function(what) {
    .Call("rir_markOptimize", what);
}

# Returns TRUE if the argument is a rir-compiled closure.
rir.isValidFunction <- function(what) {
    .Call("rir_isValidFunction", what);
}

# prints the disassembled rir function
rir.disassemble <- function(what, verbose = TRUE) {
    invisible(.Call("rir_disassemble", what, verbose))
}

# compiles given closure, or expression and returns the compiled version.
rir.compile <- function(what) {
    .Call("rir_compile", what)
}

# compiles code of the given file and returns the list of compiled version.
rir.compile.program <- function(file) {
  contents <- readChar(file, file.info(file)$size)
  expr <- eval(parse(text = paste("function() {", contents, "}", sep = "\n")))
  rir.compile(expr)
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

rir.analysis.liveness <- function(f) {
    .Call("rir_analysis_liveness", f)
}
