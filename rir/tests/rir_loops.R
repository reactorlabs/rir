compileAndTest <- function(f, expected, msg) {
    print(msg)
    pir.compile(rir.compile(f))
    res <- f()
    stopifnot(!is.null(res))
    stopifnot(!is.na(res))
    stopifnot(res == expected)
}

#################### for

f0 <- function() {
    for (i in 1:10)
        last <- i
    last
}
compileAndTest(f0, 10, "for loop")

f0a <- function() {
    for (i in 10:1)
        last <- i
    last
}
compileAndTest(f0a, 1, "for loop, descending sequence")

f0b <- function() {
    for (i in 1.2:10.1)
        last <- i
    last
}
compileAndTest(f0b, 9.2, "for loop, floats")

f0c <- function() {
    for (i in 10.5:1.6)
        last <- i
    last
}
compileAndTest(f0c, 2.5, "for loop, floats, descending sequence")

f0d <- function() {
    for (i in -1:-10)
        last <- i
    last
}
compileAndTest(f0d, -10, "for loop, negatives")

f0e <- function() {
    for (i in -10:-1)
        last <- i
    last
}
compileAndTest(f0e, -1, "for loop, negatives, ascending sequence")

f0f <- function() {
    for (i in -1.2:-10.1)
        last <- i
    last
}
compileAndTest(f0f, -9.2, "for loop, negatives, floats")

f0g <- function() {
    for (i in -10.5:-1.6)
        last <- i
    last
}
compileAndTest(f0g, -2.5, "for loop, negatives, floats, ascending sequence")

f1 <- function() {
    sum <- 0
    for (i in 1:10) {
        if (i == 5) break
        sum <- sum + i
    }
    sum
}
compileAndTest(f1, 10, "for loop: break")

f1a <- function() {
    id <- function(x) x
    sum <- 0
    for (i in 1:10) {
        if (i == 5) id(break)
        sum <- sum + i
    }
    sum
}
compileAndTest(f1a, 10, "for loop: non-local break")

f2 <- function() {
    sum <- 0
    for (i in 1:10) {
        if (i == 5) next
        sum <- sum + i
    }
    sum
}
compileAndTest(f2, 50, "for loop: next")

f2a <- function() {
    id <- function(x) x
    sum <- 0
    for (i in 1:10) {
        if (i == 5) id(next)
        sum <- sum + i
    }
    sum
}
compileAndTest(f2a, 50, "for loop: non-local next")

f3 <- function() {
    for (i in 1:3)
        if (i == 2) return(1)
    return(0)
}
compileAndTest(f3, 1, "for loop: return")

f3a <- function() {
    id <- function(x) x
    for (i in 1:3)
        if (i == 2) id(return(1))
    id(return(0))
}
compileAndTest(f3a, 1, "for loop: non-local return")

f4 <- function() {
    y <- 41
    for (i in 1:20)
        x <- y + 1
    x
}
compileAndTest(f4, 42, "for loop: constant")

f5 <- function() {
    x <- 1:10
    avg <- 0L
    for (i in 1:10)
        avg <- avg * (i - 1) / i + (x[i] / i)
    avg
}
compileAndTest(f5, 5.5, "for loop: average, with type instability")

f6 <- function() {
    x <- 1:10
    y <- 1:10
    p <- 10
    for (i in 1:10) {
        y[i] <- x[i] + x[p]
        p <- i
    }
    y
}
compileAndTest(f6, c(11, 3, 5, 7, 9, 11, 13, 15, 17, 19), "for loop: p could be optimized out")

f7 <- function() {
    id <- function(x) x
    for (i in 1:10)
        x <- if (i %% 2 == 0) {
            id(2)
        } else {
            id(33)
        }
    x + 40
}
compileAndTest(f7, 42, "for loop: branching inside")

f8 <- function() {
    for (i in 1:10)
        for (j in 1:10)
            last <- i + j
    last
}
compileAndTest(f8, 20, "for loop: double nested")

f9 <- function() {
    last <- 0
    for (i in 1:10) {
        x <- i * 10
        for (j in 1:10) {
            y <- j + 2
            last <- last + x + y
        }
    }
    last
}
compileAndTest(f9, 6250, "for loop: double nested with more operations")

f10 <- function() {
    x <- matrix(1:6, nrow=2, ncol=3, byrow=TRUE)
    y <- matrix(1:12, nrow=3, ncol=4, byrow=TRUE)

    stopifnot(ncol(x) == nrow(y))
    rows <- nrow(x)
    cols <- ncol(y)
    inner <- ncol(x)

    res1 <- x %*% y
    res2 <- matrix(0, nrow=rows, ncol=cols)
    for (c in 1:cols)
        for (r in 1:rows)
            for (i in 1:inner)
                res2[r,c] <- res2[r,c] + x[r,i] * y[i,c]
    last <- 0
    for (i in 1:10) {
        x2 <- i * 10
        for (j in 1:10) {
            y2 <- j + 2
            last <- last + x2 + y2
        }
    }
    last == 6250 && res1 == res2
}
compileAndTest(f10, TRUE, "for loop: triple nested followed by double nested")

f11 <- function() {
    x <- 0
    seq <- function() { x <<- x + 1; 1:10 }
    for (i in seq())
        last <- i
    last + x
}
compileAndTest(f11, 11, "for loop: function call in seq position")

#################### while

w0 <- function() {
    i <- 0
    while (i < 10)
        i <- i + 1
    i
}
compileAndTest(w0, 10, "while loop")

w1 <- function() {
    i <- 0
    sum <- 0
    while (i < 10) {
        i <- i + 1
        if (i == 5) break
        sum <- sum + i
    }
    sum
}
compileAndTest(w1, 10, "while loop: break")

w1a <- function() {
    id <- function(x) x
    i <- 0
    sum <- 0
    while (i < 10) {
        i <- i + 1
        if (i == 5) id(break)
        sum <- sum + i
    }
    sum
}
compileAndTest(w1a, 10, "while loop: non-local break")

# regression test
w1b <- function() {
    i <- 1
    sum <- 0
    while (i <= 10) {
        if (i == 5) break
        sum <- sum + i
        i <- i + 1
    }
    sum
}
compileAndTest(w1b, 10, "while loop: break (variant)")

w2 <- function() {
    i <- 0
    sum <- 0
    while (i < 10) {
        i <- i + 1
        if (i == 5) next
        sum <- sum + i
    }
    sum
}
compileAndTest(w2, 50, "while loop: next")

w2a <- function() {
    id <- function(x) x
    i <- 0
    sum <- 0
    while (i < 10) {
        i <- i + 1
        if (i == 5) id(next)
        sum <- sum + i
    }
    sum
}
compileAndTest(w2a, 50, "while loop: non-local next")

w3 <- function() {
    i <- 0
    while (i < 3) {
        i <- i + 1
        if (i == 2) return(1)
    }
    return(0)
}
compileAndTest(w3, 1, "while loop: return")

w3a <- function() {
    id <- function(x) x
    i <- 0
    while (i < 3) {
        i <- i + 1
        if (i == 2) id(return(1))
    }
    id(return(0))
}
compileAndTest(w3a, 1, "while loop: non-local return")

w4 <- function() {
    i <- 0
    y <- 41
    while (i < 20) {
        i <- i + 1
        x <- y + 1
    }
    x
}
compileAndTest(w4, 42, "while loop: constant")

w5 <- function() {
    i <- 0
    x <- 1:10
    avg <- 0L
    while (i < 10) {
        i <- i + 1
        avg <- avg * (i - 1) / i + (x[i] / i)
    }
    avg
}
compileAndTest(w5, 5.5, "while loop: average, with type instability")

w6 <- function() {
    i <- 0
    x <- 1:10
    y <- 1:10
    p <- 10
    while (i < 10) {
        i <- i + 1
        y[i] <- x[i] + x[p]
        p <- i
    }
    y
}
compileAndTest(w6, c(11, 3, 5, 7, 9, 11, 13, 15, 17, 19), "while loop: p could be optimized out")

w7 <- function() {
    i <- 0
    id <- function(x) x
    while (i < 10) {
        i <- i + 1
        x <- if (i %% 2 == 0) {
            id(2)
        } else {
            id(33)
        }
    }
    x + 40
}
compileAndTest(w7, 42, "while loop: branching inside")

w8 <- function() {
    i <- 0
    while (i < 10) {
        i <- i + 1
        j <- 0
        while (j < 10) {
            j <- j + 1
        }
    }
    i + j
}
compileAndTest(w8, 20, "while loop: double nested")

w9 <- function() {
    i <- 0
    last <- 0
    while (i < 10) {
        i <- i + 1
        j <- 0
        x <- i * 10
        while (j < 10) {
            j <- j + 1
            y <- j + 2
            last <- last + x + y
        }
    }
    last
}
compileAndTest(w9, 6250, "while loop: double nested with more operations")

w10 <- function() {
    x <- matrix(1:6, nrow=2, ncol=3, byrow=TRUE)
    y <- matrix(1:12, nrow=3, ncol=4, byrow=TRUE)

    stopifnot(ncol(x) == nrow(y))
    rows <- nrow(x)
    cols <- ncol(y)
    inner <- ncol(x)

    res1 <- x %*% y
    res2 <- matrix(0, nrow=rows, ncol=cols)
    c <- 1
    while (c <= cols) {
        r <- 1
        while (r <= rows) {
            i <- 1
            while (i <= inner) {
                res2[r,c] <- res2[r,c] + x[r,i] * y[i,c]
                i <- i + 1
            }
            r <- r + 1
        }
        c <- c + 1
    }
    i <- 1
    last <- 0
    while (i <= 10) {
        j <- 1
        x2 <- i * 10
        while (j <= 10) {
            y2 <- j + 2
            last <- last + x2 + y2
            j <- j + 1
        }
        i <- i + 1
    }
    last == 6250 && res1 == res2
}
compileAndTest(w10, TRUE, "while loop: triple nested followed by double nested")

w11 <- function() {
    x <- 0
    cond <- function() { x <<- x + 1; 10 }
    i <- 0
    while (i < cond())
        i <- i + 1
    i + x
}
compileAndTest(w11, 21, "while loop: function call in loop condition")

#################### repeat

# r0 skipped because we need to have a break

r1 <- function() {
    i <- 0
    repeat {
        i <- i + 1
        if (i == 10) break
    }
    i
}
compileAndTest(r1, 10, "repeat loop")

r1a <- function() {
    id <- function(x) x
    i <- 0
    repeat {
        i <- i + 1
        if (i == 10) id(break)
    }
    i
}
compileAndTest(r1a, 10, "repeat loop: non-local break")

r2 <- function() {
    i <- 0
    sum <- 0
    repeat {
        i <- i + 1
        if (i == 5) next
        if (i > 10) break
        sum <- sum + i
    }
    sum
}
compileAndTest(r2, 50, "repeat loop: next")

r2a <- function() {
    id <- function(x) x
    i <- 0
    sum <- 0
    repeat {
        i <- i + 1
        if (i == 5) id(next)
        if (i > 10) id(break)
        sum <- sum + i
    }
    sum
}
compileAndTest(r2a, 50, "repeat loop: non-local next")

r3 <- function() {
    i <- 0
    repeat {
        i <- i + 1
        if (i == 2) return(1)
        if (i == 3) break
    }
    return(0)
}
compileAndTest(r3, 1, "repeat loop: return")

r3a <- function() {
    id <- function(x) x
    i <- 0
    repeat {
        i <- i + 1
        if (i == 2) id(return(1))
        if (i == 3) id(break)
    }
    id(return(0))
}
compileAndTest(r3a, 1, "repeat loop: non-local return")

r4 <- function() {
    i <- 0
    y <- 41
    repeat {
        i <- i + 1
        x <- y + 1
        if (i == 20) break
    }
    x
}
compileAndTest(r4, 42, "repeat loop: constant")

r5 <- function() {
    i <- 0
    x <- 1:10
    avg <- 0L
    repeat {
        i <- i + 1
        avg <- avg * (i - 1) / i + (x[i] / i)
        if (i == 10) break
    }
    avg
}
compileAndTest(r5, 5.5, "repeat loop: average, with type instability")

r6 <- function() {
    i <- 0
    x <- 1:10
    y <- 1:10
    p <- 10
    repeat {
        i <- i + 1
        y[i] <- x[i] + x[p]
        p <- i
        if (i == 10) break
    }
    y
}
compileAndTest(r6, c(11, 3, 5, 7, 9, 11, 13, 15, 17, 19), "repeat loop: p could be optimized out")

r7 <- function() {
    i <- 0
    id <- function(x) x
    repeat {
        i <- i + 1
        x <- if (i %% 2 == 0) {
            id(2)
        } else {
            id(33)
        }
        if (i == 10) break
    }
    x + 40
}
compileAndTest(r7, 42, "repeat loop: branching inside")

r8 <- function() {
    i <- 0
    repeat {
        i <- i + 1
        j <- 0
        repeat {
            j <- j + 1
            if (j == 10) break
        }
        if (i == 10) break
    }
    i + j
}
compileAndTest(r8, 20, "repeat loop: double nested")

r9 <- function() {
    i <- 0
    last <- 0
    repeat {
        i <- i + 1
        j <- 0
        x <- i * 10
        repeat {
            j <- j + 1
            y <- j + 2
            last <- last + x + y
            if (j == 10) break
        }
        if (i == 10) break
    }
    last
}
compileAndTest(r9, 6250, "repeat loop: double nested with more operations")

r10 <- function() {
    x <- matrix(1:6, nrow=2, ncol=3, byrow=TRUE)
    y <- matrix(1:12, nrow=3, ncol=4, byrow=TRUE)

    stopifnot(ncol(x) == nrow(y))
    rows <- nrow(x)
    cols <- ncol(y)
    inner <- ncol(x)

    res1 <- x %*% y
    res2 <- matrix(0, nrow=rows, ncol=cols)
    c <- 1
    repeat {
        r <- 1
        repeat {
            i <- 1
            repeat {
                res2[r,c] <- res2[r,c] + x[r,i] * y[i,c]
                i <- i + 1
                if (i > inner) break
            }
            r <- r + 1
            if (r > rows) break
        }
        c <- c + 1
        if (c > cols) break
    }
    i <- 1
    last <- 0
    repeat {
        j <- 1
        x2 <- i * 10
        repeat {
            y2 <- j + 2
            last <- last + x2 + y2
            j <- j + 1
            if (j > 10) break
        }
        i <- i + 1
        if (i > 10) break
    }
    last == 6250 && res1 == res2
}
compileAndTest(r10, TRUE, "repeat loop: triple nested followed by double nested")

#################### regression

regression1 <- rir.compile(function(depth) {
    if (depth == 1) {
        1
    } else {
        x <- 0
        for (i in 1:4) {
            x <- x + regression1(depth - 1)
        }
        x
    }
})
stopifnot(regression1(4) == 64)
