foo <- function() {
    size = 10L
    sum = 0
    y = 0
    while (y < size) {
        x = 0
        while (x < size) {
            sum = 10
            x = x + 1
        }
        y = y + 1
    }
    sum
}

ex <- function() foo()

ex()
ex()
ex()
ex()
ex()
