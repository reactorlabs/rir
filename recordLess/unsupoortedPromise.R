


f <- function() {
    x <- g()
    h <- function() {
        x <<- a()
    }
    h()
    x


}


rir.compile(f)
rir.disassemble(f)
