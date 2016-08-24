f <- rir.compile(function() {
    a <- 1
    b <- 123
    while (b > 10) {
        a <- a+1
        b <- b-1
    }
    a
})

stopifnot(f() == 114);

f <- rir.compile(function() {
    while(TRUE) break
})
f()

f <- rir.compile(function() {
    while(FALSE) next
})
f()

f <- rir.compile(function() {
    i <- 1
    while(i < 10)
        if (i == 7) 
            break
        else {
            i <- i + 1
            print(i)
            next
        }
    stopifnot(i == 7)
})
f()

f <- rir.compile(function() {
    repeat break
})
f()

f <- rir.compile(function() {
    i <- 1
    repeat
        if (i == 7) 
            break
        else {
            i <- i + 1
            print(i)
            next
        }
    stopifnot(i == 7)
})
f()
