
f <- function() {

    x <- f()

    for (i in 1:100000000) {
       length(x)
       length(x)

    }
}


rir.compile(f)
rir.disassemble(f)
