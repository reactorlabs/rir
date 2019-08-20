testJit <- as.numeric(Sys.getenv("R_ENABLE_JIT", unset=2)) != 0
testJit <- testJit && (Sys.getenv("PIR_ENABLE", unset="on") == "on")
testJit <- testJit && (Sys.getenv("PIR_WARMUP", unset=3) == 3)
testJit <- testJit && (Sys.getenv("RIR_SERIALIZE_CHAOS", unset=0) == 0)
expectWarnJit <- function(code, expect) {
  warned <- FALSE
  withCallingHandlers(code, warning=function(w) warned <<- TRUE)
  stopifnot(!testJit || warned == expect)
}
expectError <- function(code) {
  stopifnot(tryCatch({ code; FALSE }, error=function(err) TRUE)) 
}

f <- rir.compile(function(x, y) x + y)
sig <- rir.mkSignature(NoSuperAssign=TRUE, NoReflection=TRUE, type="int$ int$ -> int$")
rir.setSignatureExplicit(f, sig)
f(1L, 2L)
f(3L, 4L)
f(5L, 6L)
f(7L, 8L)
stopifnot(rir.signature(f) == sig)
# TODO: Implement these assertions
# expectError(f(9, 10L)) 
x <- 1
expectWarnJit(rir.setSignature(f, NoSuperAssign=TRUE), TRUE)
# expectError(f({ x <<- 2; 11L }, 12L))
expectWarnJit(rir.setSignature(f, NoReflection=TRUE), FALSE)
# expectError(f({ parent.frame(); 13L }, 14L))
f(15L, 16L)
f(17L, 18L)
f(19L, 20L)
f(21L, 22L)
# expectError(f({ parent.frame(); 13L }, 14L))
expectWarnJit(rir.setSignature(f), TRUE)
f(15, 16)
rir.setSignature(f, NoSuperAssign=TRUE, NoReflection=TRUE, type="int$ int$ -> int$")
stopifnot(rir.signature(f) == sig)
