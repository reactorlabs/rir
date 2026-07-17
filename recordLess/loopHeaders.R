
# f <- function() {
#     i <- g()
#     while (i == 5) {
#         i <- i + 1
#     }
# }

# f <- function() {
#     a <- g()
#     while (a) {
#         i + 1
#     }
# }

# f <- function() {
#     a <- g()
#     while (b) {

#         while (a) {
#             i + 1
#         }
#     }

# }

# f <- function() {
#     while (a) {
#         x <- f()
#         for (i in x) { }  # x compiled before ClearableScopeGuard for this for-loop
#     }
# }


# f <- function() {
#     for (i in a) {
#         i + i + i

#     }
# }




rir.compile(f)
rir.disassemble(f)
