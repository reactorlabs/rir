execute <- function () {
    seed <- NaN

    resetSeed <- function() seed <<- 74755

    nextRandom <- function() {
      seed <<- bitwAnd((seed * 1309) + 13849, 65535)
      return (seed)
    }

    ballCount <- 100
    bounces   <- 0
    balls     = vector("list", length = ballCount)
    resetSeed()

    for (i in 1:ballCount) {
        random1 <- nextRandom()
        random2 <- nextRandom()
        random3 <- nextRandom()
        random4 <- nextRandom()
        balls[[i]] = c(random1 %% 500, random2 %% 500, 
                             (random3 %% 300) - 150, (random4 %% 300) - 150)
    }

    ball <- function(ball) {
        results <- bounce(ball)
        if (results[[2]]) bounces <<- bounces + 1
        return (results[[1]])
    }

    for (i in 1:50) {
      balls <- lapply(balls, ball)
    }

    return (bounces)
}

verifyResult <- function(result) {
    print (result)
    return (result == 1331);
}

bounce <- function(ball) {
    xLimit  <- 500
    yLimit  <- 500
    bounced <- FALSE

    ball[1] <- ball[1] + ball[3];
    ball[2] <- ball[2] + ball[4];

    if (ball[1] > xLimit) {
        ball[1] <- xLimit 
        ball[3] <- 0 - abs(ball[3]) 
        bounced <- TRUE
    }
    if (ball[1] < 0) {
        ball[1] <- 0
        ball[3] <- abs(ball[3])
        bounced <- TRUE
    }
    if (ball[2] > yLimit) {
        ball[2] <- yLimit
        ball[4] <- 0 - abs(ball[4])
        bounced <- TRUE
    }
    if (ball[2] < 0) {
        ball[2] <- 0 
        ball[4] <- abs(ball[4])
        bounced <- TRUE
    }
    return (list(ball, bounced))
}

for (i in 1:4) {
  r=execute()
  stopifnot(verifyResult(r))
}
