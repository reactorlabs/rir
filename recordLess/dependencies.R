






g <- function() 1L

f <- function() {
    x <- g()

    for (i in 1:20) {
        x + 1
    }

}
