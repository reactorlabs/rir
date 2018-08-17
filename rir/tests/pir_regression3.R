
f <- rir.compile(function() {
    f1 <- factor(c(1, 2, NA), exclude = NA_real_)
    f2 <- factor(c(1, 2, NA), exclude = NULL)

    print(f1)
    print(f2)
    print(nlevels(f1))
    print(nlevels(f1))
    print(nlevels(f2))
    print(nlevels(f2))
    stopifnot(identical(f1, factor(c(1,2,NA))),
              nlevels(f1) == 2, nlevels(f2) == 3,
              all(f2 == f2), !any(f2 != f2),
              identical(f1 == f1, c(TRUE,TRUE,NA)))
})

# pir.setDebugFlags(pir.debugFlags(PrintFinalPir=TRUE, ShowWarnings=TRUE, PrintEarlyPir=TRUE, PrintOriginal=TRUE, PrintFinalRir=TRUE))

f()
print(".")
f()
print(".")
f()
