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

# prints how many times the (optimized) rir function was called
rir.printInvocation <- function(what) {
    slot <- 0
    for (count in .Call("rir_invocation_count", what)) {
      cat(paste("slot ", slot, " called   ", count, "\ttimes\n"))
      slot <- slot+1
    }
}

# compiles given closure, or expression and returns the compiled version.
rir.compile <- function(what) {
    .Call("rir_compile", what)
}

# optimizes given rir compiled closure
pir.compile <- function(what, debugFlags, debugStyle, P_EARLY=FALSE, P_FINAL=FALSE, P_OPT=FALSE, WARN=FALSE) {
    debugFlags <-
        if (missing(debugFlags)) {
            if (P_EARLY)
                pir.debugFlags(PrintEarlyPir=TRUE)
            else if (P_FINAL)
                pir.debugFlags(PrintFinalPir=TRUE)
            else if (P_OPT)
                pir.debugFlags(PrintOptimizationPasses=TRUE)
            else if (WARN)
                pir.debugFlags(ShowWarnings=TRUE)
        } else {
            debugFlags
        }
    debugStyle <-
      if (missing(debugStyle)) {
        NULL
      } else {
        as.name(as.character(substitute(debugStyle)))
      }

    .Call("pir_compile",
          what,
          as.name(as.character(substitute(what))),
          debugFlags,
          debugStyle)
}

pir.tests <- function() {
    invisible(.Call("pir_tests"))
}

# returns TRUE f, when PIR compiled, satisfies the the given checks (e.g.
# environment was elided). Max assumptions compiled (+ minimal) are used, if
# warmup=<FUN> will call <FUN> repeatedly to get better assumptions.
pir.check <- function(f, ..., warmup=NULL) {
    checks <- 
        as.pairlist(lapply(lapply(as.list(substitute(...())), as.character), as.name))
    if (length(checks) == 0)
        stop("pir.check: needs at least 1 check")

    .Call("pir_check_warmup_begin")
    rir.compile(f)
    if (!is.null(warmup)) {
        rir.compile(warmup)
        pir.compile(warmup)
        for (i in 1:as.numeric(Sys.getenv("PIR_WARMUP", unset="3")))
            warmup(f)
    }
    res = .Call("pir_check", f, checks)
    .Call("pir_check_warmup_end")
    res
}

# creates a bitset with pir debug options
pir.debugFlags <- function(ShowWarnings = FALSE,
                           DryRun = FALSE,
                           PrintPassesIntoFolders = FALSE,
                           PrintIntoFiles = FALSE,
                           PrintIntoStdout = FALSE,
                           PrintInstructionIds = FALSE,
                           OmitDeoptBranches = FALSE,
                           OnlyChanges = FALSE,
                           PrintEarlyRir = FALSE,
                           PrintEarlyPir = FALSE,
                           PrintOptimizationPasses = FALSE,
                           PrintOptimizationPhases = FALSE,
                           PrintPirAfterOpt = FALSE,
                           PrintCSSA = FALSE,
                           PrintAllocator = FALSE,
                           PrintFinalPir = FALSE,
                           PrintFinalRir = FALSE) {
    # !!!  This list of arguments *must* be exactly equal to the   !!!
    # !!!    LIST_OF_PIR_DEBUGGING_FLAGS in compiler/debugging.h   !!!
    .Call("pir_debugFlags",
          ShowWarnings,
          DryRun,
          PrintPassesIntoFolders,
          PrintIntoFiles,
          PrintIntoStdout,
          PrintInstructionIds,
          OmitDeoptBranches,
          OnlyChanges,
          PrintEarlyRir,
          PrintEarlyPir,
          PrintOptimizationPasses,
          PrintOptimizationPhases,
          PrintPirAfterOpt,
          PrintCSSA,
          PrintAllocator,
          PrintFinalPir,
          PrintFinalRir,
          # wants a dummy parameter at the end for technical reasons
          NULL)
}

# sets the default debug options for pir compiler
pir.setDebugFlags <- function(debugFlags = pir.debugFlags()) {
    invisible(.Call("pir_setDebugFlags", debugFlags))
}

# compiles code of the given file and returns the list of compiled version.
pir.program <- function(file) {
  contents <- readChar(file, file.info(file)$size)
  expr <- eval(parse(text = paste("function() {", contents, "}", sep = "\n")))
  rir.compile(expr)
  for (i in 1:as.numeric(Sys.getenv("PIR_WARMUP", unset="3")))
    expr()
}

rir.eval <- function(what, env = globalenv()) {
    .Call("rir_eval", what, env);
}

# returns the body of rir-compiled function. The body is the vector containing its ast maps and code objects
rir.body <- function(f) {
    .Call("rir_body", f);
}

# prints invocation during evaluation
# insert a call to .printInvocation()' in R code and the invocation count of the
# enclosing function will be printed
.printInvocation <- function() {
    cat("Invocation count: 0 (not compiled)\n")
}

# breakpoint during evaluation
# insert a call to `.int3()` in R code and get a breakpoint when it is evaluated
# note: the actual body of this function is replaced by an "int3_" bytecode
.int3 <- function() {
    stop("missed breakpoint, did you re-compile RIR?")
}

# Serializes the SEXP, preserving RIR/PIR-compiled closures, to the given path
rir.serialize <- function(data, path) {
    .Call("rir_serialize", data, path)
}

# Deserializes and returns the SEXP at the given path
rir.deserialize <- function(path) {
    .Call("rir_deserialize", path)
}

rir.enableLoopPeeling <- function() {
    .Call("rirEnableLoopPeeling")
}

rir.disableLoopPeeling <- function() {
    .Call("rirDisableLoopPeeling")
}

rir.enableEventCounters <- function() {
    .Call("rirEnableEventCounters")
}

rir.disableEventCounters <- function() {
    .Call("rirDisableEventCounters")
}

rir.printBuiltinIds <- function() {
    invisible(.Call("rirPrintBuiltinIds"))
}