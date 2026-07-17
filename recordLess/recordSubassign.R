g <- function() {

}

f <- function() {

    x <- c(2L, 3L)
    #x
    x[[1]] <- g()
    #x
    x[[1]] <- h()


}

rir.compile(f)
rir.disassemble(f)
