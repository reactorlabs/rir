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

f <- rir.compile(function() {
    for (i in 1:10) next
})
f()

f <- rir.compile(function() {
    for (i in 1:10) break
})
f()

f <- rir.compile(function() {
    a <- 3
    for (i in 1:10) a <- i
    stopifnot(a == 10)
})
f()

f <- rir.compile(function() {
    i <- 1
    for (i in 1:123)
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
    for (i in 1:10) {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{next}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
})
f()
f <- rir.compile(function() {
    for (i in 1:2) {{{{{
    for (i in 1:2) {{{{{
    for (i in 1:2) {{{{{
    for (i in 1:2) {{{{{
    for (i in 1:2) {{{{{
    for (i in 1:2) {{{{{
    for (i in 1:2) {{{{
    for (i in 1:2) {
    for (i in 1:2) {
        for (i in 1:2) next}
    }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
})
f()

g <- rir.compile(function(r) r())

f <- rir.compile(function() {
    for (i in 1:3)
        g(next)
})
f()

f <- rir.compile(function() g(return)) 
f()

ff <- rir.compile(function() {
    f()
})
ff()

f <- rir.compile(function() g(return)) 
g <- rir.compile(function(r) h(r)) 
h <- rir.compile(function(r) r())
ff <- rir.compile(function() {
    for (i in 1:10) {
        print(i)
        f()
    }
    f()
})
ff()

stopifnot(rir.compile(function() seq(5,10,2))() == c(5, 7, 9))
stopifnot(rir.compile(function() seq(5L,10L,2L))() == c(5L, 7L, 9L))
stopifnot(rir.compile(function() seq(10L,4L,-2L))() == c(10L, 8L, 6L, 4L))

f <- rir.compile(function() {
    s <- 0
    for (i in seq(5,10,2))
        s <- s + i
    s
})
stopifnot(f() == 21)
