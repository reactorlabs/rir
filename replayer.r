recordings.csv <- function(r) {
  if (is.character(r)) {
      r <- recordings.load(r)
  }

  result <- ""

  # intersperse a vector with commas
  insert.commas <- function(vec) {
    flag <- TRUE

    lapply(vec, function(el) {
      if (flag){
          flag <<- FALSE
          el
      } else {
          paste0(",", el)
      }
    })
  }

  # output a line of vector to result
  line <- function(vec) {
    # Surround with quotes if it contains commas
    vec <- lapply(vec, function(el) {
      if (grepl(",", el)) {
        paste0('"', gsub('"', '\\"', el), '"')
      } else {
        el
      }
    })
    vec <- insert.commas(vec)
    vec <- paste(vec, collapse="")
    result <<- paste0(result, vec, "\n")
  }

  columns <- c("idx", "type", "fun", "env", "ctx", "speculative_ctx", "speculative", "reason", "bitcode_len", "invocation_delta", "deopt_delta", "changed", "is_promise")

  line(columns)

  get_fun <- function(idx) {
    f <- r$functions[[ as.integer(idx) + 1 ]]
    c(f$name, f$env)
  }

  pp <- function(obj, type) {
    recordings.printEventPart(obj, type, r$functions)
  }

  idx <- 1

  for (e in r$events) {
    event <- NULL

    event$idx <- idx
    idx <- idx + 1

    if (class(e) == "event_compile") {
      if (!e$succesful) {
          next
      }

      event$type <- "Compilation"

      f <- get_fun(e$closure)
      event$fun <- f[1]
      event$env <- f[2]

      event$ctx <- pp(e$dispatch_context, "context")

      event$speculative <- paste(insert.commas(lapply(
        e$speculative_contexts,
        function(spec) pp(spec, "speculative")
      )), collapse="")

      reasAcc <- ""

      for (cr in c("heuristic", "condition", "osr" )) {
        reas <- e[[ paste0("compile_reason_", cr) ]]
        if (!is.null(reas)){
          if (nchar(reasAcc) == 0) {
            reasAcc <- paste0(cr, "=", class(reas))
          } else {
            reasAcc <- paste0(reasAcc, ",", cr, "=", class(reas))
          }
        }
      }

      event$reason <- reasAcc

      event$bitcode_len <- nchar(e$bitcode)

    } else if (class(e) == "event_deopt") {
      event$type <- "Deopt"

      f <- get_fun(e$dispatchTable)
      event$fun <- f[1]
      event$env <- f[2]

      event$ctx <- pp(e$version, "context")

      if ( e$reason_promise_idx >= 0 ){
          reason <- paste0( "Promise ", e$reason_promise_idx )
      } else {
          reason <- "Baseline"
      }

      reason <- paste0("(", reason, ",offset=", e$reason_code_off, ")")

      event$reason <- paste0(pp(e$reason, "deopt_reason"), reason)
    } else if (class(e) == "event_invocation") {
      event$type <- "Invocation"

      f <- get_fun(e$dispatchTable)
      event$fun <- f[1]
      event$env <- f[2]

      event$ctx <- pp(e$context, "context")

      event$invocation_delta <- e$deltaCount
      event$deopt_delta <- e$deltaDeopt
      event$reason <- pp(e$source, "invocation_source")
    } else if (class(e) == "event_sc") {
      event$type <- "SpeculativeContext"

      f <- get_fun(e$dispatchTable)
      event$fun <- f[1]
      event$env <- f[2]

      event$speculative <- paste0(pp( e$sc, "speculative" ), "@", e$index)

      event$is_promise <- e$is_promise
      event$changed <- e$changed
    } else {
      event$type = paste0("[", class(e), "]")
    }

    vec <- lapply(columns, function(col) {
        if (is.null(event[[ col ]]))
            ""
        else
            event[[ col ]]
    })

    line(vec)
  }
  result
}
