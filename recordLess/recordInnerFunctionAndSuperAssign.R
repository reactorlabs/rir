
# q <- function() 1


# f <- function(a) {

#     g <- function(b) {
#         if (b)
#             a <- q()

#         a + a
#         # for (i in 1:20000000) {
#         #     a + a

#         # }
#     }
#     g

# }

# rir.compile(f)

# rir.disassemble(f)
# rir.disassemble(f(4))

# f <- function(a) {

#     #a <- q()
#     g <- function() {

#         h <- function() {
#             for (i in 1:20) {
#                 a + a

#             }
#         }
#         h
#     }


#     g()
# }
# rir.compile(f)
# rir.disassemble(f)
# rir.disassemble(f())




# f <- function(a) {

#     g <- function(b) {
#         if (b)
#             a <- q()

#         a + a

#     }
# }

# rir.compile(f)
# rir.disassemble(f)
# rir.disassemble(f(1))


# q <- function() 1
# f <- function() {
#     a <- q()

#     g <- function(b) {

#         a + a

#     }
# }



# # super assign excluded from compilation of outer function (simple case)
# f <- function() {
#     x <- g()
#     h <- function() {
#         x <<- a()
#     }
#     h()
#     x
# }


# super assign excluded from controlled set passed to inner
f <- function() {
    x <- 1
    g <- function() { x <<- "hello" }
    h <- function() { x; g(); x }
}






rir.compile(f)
rir.disassemble(f)
