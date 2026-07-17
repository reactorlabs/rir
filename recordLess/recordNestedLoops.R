
# f <- function() {

#     y <- h()
#     for (i1 in q1) {
#         x <- g()
#         h <- s()
#         #clear [bit 1, bit 2]
#         for (i2 in q2) {

#             d <- ww()
#             #clear [bit 3]

#             for (i3 in q3) {
#                 h #[bit 1]
#                 d #[bit 3]
#                 y #[bit 0]
#                 x #[bit 2]

#             }

#         }
#     }
# }



f <- function() {

    for (i0 in q0) {
        y <- h()
        for (i1 in 1:4) {
            y
            y
            i1
            i1

        }

        x <- h()
        for (i1 in q1) {
            x
            x

        }

    }
}


rir.compile(f)
rir.disassemble(f)
