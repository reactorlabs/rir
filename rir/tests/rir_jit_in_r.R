install.packages("R6", repos = "http://cran.us.r-project.org")
library(R6)

out <- c()
catOut <- function(new) {
  out <<- append(out, c(new))
}

rir.callLocId <- NULL
rir.onModifys <- new.env()

rir.onModify <- function(x, env, new) {
  key <- paste(deparse(x), "|", capture.output(str(env)))
  if (exists(key, envir = rir.onModifys)) {
    rir.onModifys[[key]](new)
  }
}

`rir.onModify<-` <- function(x, env, value) {
  key <- paste(deparse(x), "|", capture.output(str(env)))
  prev <- {
    if (exists(key, envir = rir.onModifys)) {
      rir.onModifys[[key]]
    } else {
      function(new) { }
    }
  }
  rir.onModifys[[key]] <<- function(new) {
    prev(new)
    value(new)
  }
}

nextCheckId <- 1

Check <- R6Class(
  "Check",
  private = list(
    id = NA,
    needsToSkip = NULL
  ),
  public = list(
    arg = NULL,
    numPasses = 0,
    skip = FALSE,
    initialize = function(arg, needsToSkip) {
      self$arg <- arg
      private$id <- nextCheckId
      nextCheckId <<- nextCheckId + 1
      private$needsToSkip <- needsToSkip
    },
    doesPass = function(env) {
      stop("not implemented")
    },
    tryOptimize = function(env) {
      stop("not implemented")
    },
    run = function(eargs, args, env) {
      earg <- eargs[[self$arg]]
      arg <- args[[self$arg]]
      if (self$skip) {
        catOut(paste(private$id, "-", "Skip"))
        TRUE
      } else if (self$doesPass(earg, arg, env)) {
        catOut(paste(private$id, "-", "Pass"))
        self$numPasses <- self$numPasses + 1
        self$tryOptimize(earg, arg, env)
        self$skip || !private$needsToSkip
      } else {
        catOut(paste(private$id, "-", "Fail"))
        self$numPasses <- 0
        FALSE
      }
    }
  )
)

CheckInteger <- R6Class(
  "CheckInteger",
  inherit = Check,
  public = list(
    initialize = function(arg = NA) {
      super$initialize(deparse(substitute(arg)), needsToSkip = FALSE)
    },
    doesPass = function(earg, arg, env) {
      is.numeric(eval(earg, env))
    },
    tryOptimize = function(earg, arg, env) {
      if (is.atomic(arg)) {
        self$skip = TRUE
      } else if (is.symbol(arg) && self$numPasses >= 3) {
        self$skip = TRUE
        used <- FALSE
        rir.onModify(arg, env) <- function(new) {
          if (!used) {
            self$skip = FALSE
            used <<- TRUE
          }
        }
      }
    }
  )
)

CheckConstant <- R6Class(
  "CheckConstant",
  inherit = Check,
  public = list(
    initialize = function(arg = NA) {
      super$initialize(deparse(substitute(arg)), needsToSkip = TRUE)
    },
    doesPass = function(earg, arg, env) {
      is.atomic(arg) || (is.symbol(arg) && !hasActiveBinding(env, arg))
    },
    tryOptimize = function(earg, arg, env) {
      if (is.atomic(arg)) {
        self$skip = TRUE
      } else if (is.symbol(arg) && self$numPasses >= 3) {
        self$skip = TRUE
        used <- FALSE
        rir.onModify(arg, env) <- function(new) {
          if (!used) {
            self$skip = FALSE
            used <<- TRUE
          }
        }
      }
    }
  )
)

every <- function(col, f) {
  for (x in col) {
    if (!f(x)) {
      return (FALSE)
    }
  }
  TRUE
}

specialize <- function(checks, fast, slow, args, env, eargs = parent.frame()) {
  if (every(checks, function(check) check$run(eargs, args, env))) {
    fast
  } else {
    slow
  }
}

envBindings <- vector("list", length=1000)
envBindToBC <- function(env, name, mkX) {
  makeActiveBinding(name, function() {
    if (is.null(envBindings[[rir.callLocId]])) {
      envBindings[[rir.callLocId]] <<- mkX()
    }
    envBindings[[rir.callLocId]]
  }, env)
}

f <- rir.compile(function() {
  rir.enablePrototype()
  j <- 1
  j <- 2
  rir.disablePrototype()
  j
})
stopifnot(f() == 2)

f <- rir.compile(function(n=5) {
  rir.enablePrototype()

  envBindToBC(environment(), "add", function() {
    checks <- c(
      CheckInteger$new(x),
      CheckInteger$new(y)
    )
    cached <- NULL
    function(x, y) {
      args <- new.env()
      args$x <- substitute(x)
      args$y <- substitute(y)
      specialize(
        checks,
        fast = {
          x + y
        },
        slow = {
          x + y
        },
        args = args,
        env = parent.frame()
      )
    }
  })

  j <- 1
  for (i in 1:n) {
    rir.callLocId <<- 1
    add(i, i)
    rir.callLocId <<- 2
    add(j, 1)
  }
  rir.disablePrototype()
})
f()
print(out)
stopifnot(out == c(
  "1 - Pass",
  "2 - Pass",
  "3 - Pass",
  "4 - Pass",
  "1 - Pass",
  "2 - Pass",
  "3 - Pass",
  "4 - Skip",
  "1 - Pass",
  "2 - Pass",
  "3 - Pass",
  "4 - Skip",
  "1 - Pass",
  "2 - Pass",
  "3 - Skip",
  "4 - Skip",
  "1 - Pass",
  "2 - Pass",
  "3 - Skip",
  "4 - Skip"
))

out <- c()
f <- rir.compile(function(n=5) {
  rir.enablePrototype()

  add <- `+`
  envBindToBC(environment(), "+", function() {
    checks <- c(
      CheckConstant$new(x),
      CheckConstant$new(y)
    )
    function(x, y) {
      args <- new.env()
      args$x <- substitute(x)
      args$y <- substitute(y)
      cached <- NULL
      specialize(
        checks,
        fast = {
          if (is.null(cached)) {
            cached <<- x + y
          }
          cached
        },
        slow = {
          cached <<- NULL
          x + y
        },
        args = args,
        env = parent.frame()
      )
    }
  })

  j <- 1
  res <- 0
  for (i in 1:n) {
    rir.callLocId <<- 1
    res <- add(res, add(1, i))
    rir.callLocId <<- 2
    res <- add(res, add(1, j))
  }
  rir.disablePrototype()
  res
})
stopifnot(f() == 30)
stopifnot(out == c(
  "1 - Pass",
  "2 - Pass",
  "3 - Pass",
  "4 - Pass",
  "1 - Pass",
  "2 - Pass",
  "3 - Pass",
  "4 - Skip",
  "1 - Pass",
  "2 - Pass",
  "3 - Pass",
  "4 - Skip",
  "1 - Pass",
  "2 - Pass",
  "3 - Skip",
  "4 - Skip",
  "1 - Pass",
  "2 - Pass",
  "3 - Skip",
  "4 - Skip"
))


