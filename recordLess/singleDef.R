
# f <- function() {
#     x <- f()
#     x + 1L
# }


f <- function() {

    x <- f()

    if (a)
        x + 1L
}


f <- function() {

    if (a)
        x <- f()

    x + 1L



}
##################################

f <- function(b) {
    x <- g()
    x + 1L
}


f <- function(b) {
    x <- g()

    if (b)
        x + 1L
}



f <- function(b) {
    x + 1L
}


f <- function(b) {
    if (b)
        x <- g()
    else
        x <- h()

    x + 1L
}

###################





# f <- function() {

#     x <- g()   #L1

#     while (b) {
#         x
#     }
# }



# f <- function(s) {
#     x <- g()
#     x
#     while (a) {
#         x <- f()
#         x

#     }
#     x

# }


# f <- function() {
#     if (a) {
#         x <- f()
#     }
#     x
# }


# f <- function() {
#     x <- f()
#     if (a) {
#         x
#     }
#     x
# }





# f <- function() {
#     while (a) {
#         x <- g()
#         x
#     }
# }







# f <- function() {

#     if (b)
#         x <- g()

#     while (a) {
#         x
#     }
# }

# f <- function() {

#     while (a) {
#         if (b)
#             x <- g()

#         x
#     }
# }



# f <- function() {

#     if (b)
#         x <- g()
#     else
#         x <- h()

#     while (a) {
#         x
#     }
# }

# f <- function(s) {
#     x <- getValueOfType(s)

#     if (s == "int") {
#         x + 1L
#     }

# }


# f <- function() {
#     if (a) {
#         x <- f()
#         if (b)
#             x
#     }

#     if (b) {
#         x
#     }

# }


f <- function() {

    while (b) {
        x <- g()

        while (b) {
            x
        }
    }
}


# f <- function() {

#     x <- f()
#     while (b) {
#         x
#         while (d) {
#             x <- f()
#         }
#     }
# }



# f <- function() {
#     x <- f()
#     if (b) {
#         x + x
#     }
#     x
# }



# f <- function() {

#     x <- f()
#     if (b) {
#         a <- x + x
#     }
#     x
# }



f <- function() {

    a <- f()
    b <- f()
    c <- f()
    d <- f()

    for (i in 1:100000000) {
         ((((a + 2L) * b) + c) + d) * ((((a + 2L) * b) + c) + d)


    }
}



# f <- function() {
#     x <- f()
#     while (a) {
#         x <- g()
#     }
#     x
# }



rir.compile(f)
rir.disassemble(f)
