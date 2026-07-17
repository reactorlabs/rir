
g <- function(x){ x}

f <- function(a) {

    for (i in 1:20000000) {

       g({
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a

       })

    }
}


rir.compile(g)

rir.compile(f)
#rir.disassemble(f)

f(1L)



#rir.disassemble(f)
