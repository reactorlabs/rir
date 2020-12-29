if (Sys.getenv("PIR_ENABLE_PROFILER") != "1") {
  print("test only works with profiler enabled")
  q()
}

f = function() a+a+a+a+a+1L
rir.compile(f)
a = 1
f()
a = 1L
f()
rir.disassemble(f)
rir.markFunction(f, Reopt=TRUE)
f()
rir.disassemble(f)
for (i in 1:1000000)
  f()
rir.disassemble(f)

# assert f was reoptimized, ie. we expect to see not all of the
# invocations counted, since the optimized code object was replaced
stopifnot(sum(rir.functionInvocations(f)) < 1000000)
