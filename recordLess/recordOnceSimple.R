

f <- function() {


    while (b) {
        a <- g()
        while (b) {
            a
        }
    }
}


rir.compile(f)
rir.disassemble(f)
