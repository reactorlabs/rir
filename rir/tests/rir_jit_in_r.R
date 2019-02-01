install.packages("R6", repos = "http://cran.us.r-project.org")
library(R6)

out <- c()
catOut <- function(new) {
  out <<- append(out, c(new))
}

rir.srcloc <- NULL
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
      function() { }
    }
  }
  rir.onModifys[[key]] <<- function() {
    prev()
    value()
  }
}

nextCheckId <- 1

Check <- R6Class(
  "Check",
  private = list(
    id = NA
  ),
  public = list(
    numPasses = 0,
    skip = FALSE,
    initialize = function() {
      private$id <- nextCheckId
      nextCheckId <<- nextCheckId + 1
    },
    doesPass = function(env) {
      stop("not implemented")
    },
    tryOptimize = function(env) {
      stop("not implemented")
    },
    run = function(env, args, ext) {
      if (self$skip) {
        catOut(paste(private$id, "-", "Skip"))
        TRUE
      } else if (self$doesPass(env)) {
        catOut(paste(private$id, "-", "Pass"))
        self$numPasses <- self$numPasses + 1
        self$tryOptimize(args, ext)
        TRUE
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
    arg = NULL,
    initialize = function(arg = NA) {
      self$arg <- substitute(arg)
      super$initialize()
    },
    doesPass = function(env) {
      is.numeric(eval(self$arg, env))
    },
    tryOptimize = function(args, etx) {
      arg <- args[[deparse(self$arg)]]
      if (is.atomic(arg)) {
        self$skip = TRUE
      } else if (is.symbol(arg) && self$numPasses >= 3) {
        self$skip = TRUE
        used <- FALSE
        rir.onModify(arg, etx) <- function() {
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

specialize <- function(checks, fast, slow, args, ext) {
  env <- parent.frame()
  if (every(checks, function(check) check$run(env, args, ext))) {
    fast
  } else {
    slow
  }
}

envBindings <- vector("list", length=1000)
envBindLocal <- function(env, name, mkX) {
  makeActiveBinding(name, function() {
    if (is.null(envBindings[[rir.srcloc]])) {
      envBindings[[rir.srcloc]] <<- mkX()
    }
    envBindings[[rir.srcloc]]
  }, env)
}

envBindLocal(environment(), "add", function() {
  checks <- c(
    CheckInteger$new(x),
    CheckInteger$new(y)
  )
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
      ext = parent.frame()
    )
  }
})

f <- rir.compile(function() {
  j <- 1
  j <- 2
  j
})
stopifnot(f() == 2)

f <- rir.compile(function(n=5) {
  rir.enablePrototype()
  j <- 1
  for (i in 1:n) {
    rir.srcloc <<- 1
    add(i, i)
    rir.srcloc <<- 2
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
