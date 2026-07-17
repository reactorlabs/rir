f <- function(b) {
    b
    a <- 2
    f(a) + f(b + d)
}



b <- x
b

b <- x
if (a) {
    b <- y
}
b



b <- x
if (a) {
    b
}



b <- x
while (a) {
    b
}


function(b) {
    while (x) {
        b
    }
}
=======================


function(b) {
    if (a) {
        b
    }
}
b should RECORD




function(b) {
    b
    if (a) {
        b
    }
}
first b should RECORD
second b should RECORD


function(b) {
    if (a) {
        b
    }

    if (a) {
        b
    }
    b
}
RECORD, RECORD, RECORD


function(b) {
    b
    if (a) {
        b
    }
    b
}
first b should RECORD
second b should RECORD
third b should SKIP - copy from first b

function(b) {
    if (a) {
        b
        if (a) {
            b
        }
        b
    }
}
first b should RECORD
second b should RECORD
third b should SKIP - copy from first b




function(b) {
    if (a) {
        b
    }
    b
    b
    b
    b
}
first b should RECORD -
second  b should RECORD -
third b should not record (SKIP and copy from second b)


-------------
