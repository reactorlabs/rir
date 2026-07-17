
f <- function() {

    a <- 1L
    b <- 1L
    c <- 1L
    d <- 1L


    for (i in 1:40000000) {
        x <- ((((a + 2L) * b) + c) + d) * ((((a + 2L) * b) + c) + d)
    }
}


rir.compile(f)
rir.disassemble(f)

f()

rir.disassemble(f)
