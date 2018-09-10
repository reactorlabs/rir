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
pir.compile <- function(what, debugFlags) {
    .Call("pir_compile", what, if (missing(debugFlags)) NULL else debugFlags)
}

pir.tests <- function() {
    invisible(.Call("pir_tests"))
}

# creates a bitset with pir debug options
pir.debugFlags <- function(ShowWarnings = FALSE,
                           DryRun = FALSE,
                           PreserveVersions = FALSE,
                           PrintIntoFiles = FALSE,
                           PrintIntoStdout = FALSE,
                           PrintEarlyRir = FALSE,
                           PrintEarlyPir = FALSE,
                           PrintOptimizationPasses = FALSE,
                           PrintCSSA = FALSE,
                           PrintAllocator = FALSE,
                           PrintFinalPir = FALSE,
                           PrintFinalRir = FALSE) {
    # !!!  This list of arguments *must* be exactly equal to the   !!!
    # !!!    LIST_OF_PIR_DEBUGGING_FLAGS in compiler/debugging.h   !!!
    .Call("pir_debugFlags", ShowWarnings, DryRun, PreserveVersions,
          PrintIntoFiles, PrintIntoStdout, PrintEarlyRir, PrintEarlyPir,
          PrintOptimizationPasses, PrintCSSA, PrintAllocator, PrintFinalPir,
          PrintFinalRir,
          # wants a dummy parameter at the end for technical reasons
          NULL)
}

# sets the default debug options for pir compiler
pir.setDebugFlags <- function(debugFlags = pir.debugFlags()) {
    invisible(.Call("pir_setDebugFlags", debugFlags))
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
# insert a call to `.debug.break()` in R code and get a breakpoint when it is evaluated
# note: the actual body of this function is replaced by an "int3_" bytecode
.debug.break <- function() {
    stop("missed breakpoint, did you re-compile RIR?")
}
