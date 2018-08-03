# the following functions are intended for the API

rir.markOptimize <- function(what) {
    .Call("rir_markOptimize", what);
}

# Returns TRUE if the argument is a rir-compiled closure.
rir.isValidFunction <- function(what) {
    .Call("rir_isValidFunction", what);
}

# prints the disassembled rir function
rir.disassemble <- function(what, verbose = FALSE) {
    invisible(.Call("rir_disassemble", what, verbose))
}

# compiles given closure, or expression and returns the compiled version.
rir.compile <- function(what) {
    .Call("rir_compile", what)
}

# optimizes given rir compiled closure
pir.compile <- function(what, verbose = FALSE, dryRun = FALSE) {
    .Call("pir_compile", what, verbose, dryRun)
}

pir.tests <- function() {
    invisible(.Call("pir_tests"))
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

# breakpoint during evaluation
# insert a call to `.debug.break()` in R code and get a breakpoint when the function is evaluated
.debug.break <- function() {
    .Call("debug_break")
}
