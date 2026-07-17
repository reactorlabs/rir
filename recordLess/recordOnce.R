f <- function(b) {
    x <- g(b) [R]
    x [copy ]
}


f <- function() {
    x <- g()
    error()
    x
}


##########


f <- function() {
    x <- g()   #L1
    x [copy from L1]
}


f <- function(b) {
    if (b)
        x <- g()
    else
        x <- h()
    x [record]
}

#####


f <- function() {

    while () {
        x <- g()   #L1
        x [copy from L1]
    }
}


f <- function() {

    x <- g()   #L1

    while (b) {
        x [ONCE: copy from L1]
    }
}


f <- function() {

    if (b)
        x <- g()
    else
        x <- h()

    while () {
        x [ONCE: record]
    }
}


######


f <- function() {

    while (b) {
        x <- g()  #L1 . reset fired flag

        while (b) {
            x
            [ONCE: copy from L1]
        }
    }
}





# How to avoid zeroing
# bitmap indexing


###
 f <- function(s) {

    while (cond()) {

        x <- getSomeIntOrString(s)

        if (s == "int") {
            x + 1L
        }

    }

 }


f(FALSE)

########

f <- function(s) {
    x <- getValueOfType(s)

    if (s == "int") {
        x + 1L
    }

}

f("float")


####



f <- function(s) {
    while (){ {
        x <- getValueOfType(s)

        if (s == "int") {
            x + 1L
        }
    }

}


f <- function(s) {
    while (){ {
        x <- f()
        x

    }

}



f <- function() {
    if (a) {
        x <- f()
        if (b)
            x
    }

    if (b) {
        x
    }

}
###########

# pos dom , but NO unique def reach
if (a) {
    x <- f()
}
x


# NO post dom,  unique def reach
# use has a subset of the values of x's DEF
x <- f()
if (a) {
    x
}
