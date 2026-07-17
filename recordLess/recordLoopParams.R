    # a <- 1L
    # b <- 1L
    # c <- 1L
    # d <- 1L

#x <- ((((a + 2L) * b) + c) + d) * ((((a + 2L) * b) + c) + d)


f <- function(a,b,c,d) {

    for (i in 1:100000000) {
       a
       b
       c
       d


    }
}


rir.compile(f)
rir.disassemble(f)

f(1L,1L,1L,1L)



rir.disassemble(f)
