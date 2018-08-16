pir.setDebugFlags(pir.debugFlags(PrintFinalPir=TRUE, ShowWarnings=TRUE, PrintEarlyPir=TRUE, PrintOriginal=TRUE, PrintFinalRir=TRUE))

print(callCC)
callCC <- rir.compile(callCC)
rir.disassemble(callCC)
callCC(function(k) 1)
callCC(function(k) k(1))
callCC(function(k) {k(1); 2})
callCC(function(k) repeat k(1))
