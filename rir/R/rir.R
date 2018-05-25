# The following functions are intended for the API

# Prints the disassembled rir function
rir.disassemble <- function(what, verbose = FALSE) {
    invisible(.Call("rir_disassemble", what, verbose))
}

# Compiles given closure, or expression and returns the compiled version.
rir.compile <- function(what) {
    .Call("rir_compile", what)
}

rir.eval <- function(what, env = globalenv()) {
    .Call("rir_eval", what, env);
}

# optimizes given rir compiled closure
pir.optimize <- function(what, verbose = FALSE) {
    .Call("pir_optimize", what, verbose)
}

# compiles code of the given file and returns the list of compiled version.
rir.compile.program <- function(file) {
  contents <- readChar(file, file.info(file)$size)
  expr <- eval(parse(text = paste("function() {", contents, "}", sep = "\n")))
  rir.compile(expr)
}
