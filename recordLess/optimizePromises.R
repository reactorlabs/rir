
# f <- function() {


#     a <- g()
#     g({

#         a <- k()
#         g({
#             a <- h()
#             while (b) {
#                 a
#             }
#             a
#         })
#         a

#     })
#     a


# }

f <- function(x) {

    g({
        while (b) {
            x
            x
        }
    })
}



# f <- function() {
#     a <- f()
#     g({
#         while (b)
#             a
#     })
#     a <- g()
# }

# f <- function(x) {

#     while (b) {
#         f(x + x)
#     }

# }



# f <- function() {
#     g({
#         a <- f()
#         while (b)
#             a + a
#         a
#     })
# }


# f <- function() {
#     while (b) {
#         d;
#         g({
#             while (q)
#                 x;

#         })
#     }
# }




rir.compile(f)
