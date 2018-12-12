test <- function() {
   seed <- 74755

   nextRandom <- function() {
       seed <<- bitwAnd((seed * 1309) + 13849, 65535)
       return (seed)
   }

   buildTreeDepth <- function(depth, random) {
       if (depth == 1) {
           return (c(nextRandom() %% 10 + 1))
       } else {
           array <- vector("list", length = 4)
           for (i in 1:4) {
               array[[i]] <- buildTreeDepth(depth - 1, random)
           }
           return (array)
       }
   }

   buildTreeDepth(2, nextRandom())
}

f <- rir.compile(function() test())

f()
pir.compile(test, debugFlags=pir.debugFlags(PrintPirAfterOpt=TRUE, ShowWarnings=TRUE))
