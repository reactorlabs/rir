
g <- function(x){ x}

f <- function(a,b,c,d,e,f,g,h,i,j,k) {

    for (i in 1:20000000) {

       g({
            a
            b
            c
            d
            e
            f
            g
            h
            i
            j
            k
       })

    }
}


rir.compile(g)

rir.compile(f)
rir.disassemble(f)

f(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L)



rir.disassemble(f)
