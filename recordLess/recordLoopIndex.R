
# f <- function() {

#     for (i in 1:100000000) {
#         for (j in 1:100000000) {

#             if (x)
#                 j
#             if (b)
#                 j
#         }
#     }

# }


# f <- function() {
#   for (i in 1:3) {
#     for (j in 1:4) {
#       if (TRUE) j
#       if (TRUE) j
#     }
#   }
# }

# f <- function() {
#   x <- g()
#   for (i in 1:3) {
#     x
#     y

#   }
# }

# f <- function() {

#     for (i in 1:nrow(x)) {
#         i
#     }

#     # for (i in 1:x) {
#     #     i
#     # }
# }

# f <- function() {

#     # no need to clear bits for i
#     for (i in 1:x) {
#         # clear bit 1 (for j)
#         for (j in 1:x) {
#             # clear bits 2 and 3 (for k)
#             for (k in 1:x) {
#                 if (b)
#                     k # [ record once - bit 2]
#                 i # [record once - bit 0]
#                 j # [ record once bit 1]
#                 if (b)
#                     k # [ record once - bit 3 ]
#             }
#         }
#     }

# }



f <- function() {
  for (i in 1:3) {
    for (j in 1:4) {
        j

    }
  }
}

rir.compile(f)
rir.disassemble(f)
