recordings.csv <- function(r, out = "") {
  startT <- Sys.time()

  library(parallel)

  if (is.character(r)) {
    r <- recordings.load(r)
  }

  # Define the catf
  if (out != "") {
    file_conn <- file(out, open = "wt")
    catf <- function(str)
      cat(str, file = file_conn, append = TRUE)
    on.exit(close(file_conn))
  } else {
    catf <- function(str) cat(str)
  }

  # intersperse a vector with commas
  format.vector <- function(vec) {
    first <- TRUE
    vec <- lapply(vec, function(el) {
      if (first) {
        first <<- FALSE
        el
      } else {
        paste0(",", el)
      }
    })

    paste( vec, collapse="" )
  }

  # Surround with quotes if it contains commas
  quote.comma <- function(str) {
    if (grepl(",", str)) {
      paste0('"', gsub('"', '\\"', str), '"')
    } else {
      str
    }
  }

  # output a line of CSV - one event
  line <- function(event) {
    first <- TRUE
    lapply(event, function(el)
      if (first) {
        first <<- FALSE
        catf(quote.comma(el))
      } else {
        catf(paste0(",", quote.comma(el)))
      }
    )

    catf("\n")
  }

  pp <- function(obj, type) {
    recordings.printEventPart(obj, type, r$functions)
  }

  columns <- c("idx", "type", "fun", "env", "ctx",
               "speculative_ctx", "speculative", "call_ctx",
               "reason", "bitcode_len", "pir_len", "changed",
               "is_promise", "is_native", "callee_address",
               "missing_asmpt")


  cores <- detectCores()

  pb <- txtProgressBar(min = 0, max = length(r$events), style = 3)
  on.exit(close(pb))

  events <- mclapply(seq_along(r$events), function(idx) {
    e <- r$events[[ idx ]]

    event <- setNames(as.list(rep("", length(columns))), columns)
    event$idx <- toString(idx)

    f <- r$functions[[as.integer(e$funIdx) + 1]]
    event$fun <- f$name
    event$env <- f$env

    cl <- class(e)

    if (cl == "event_compile_start") {
      event$type <- "CompilationStart"

      reasAcc <- ""

      for (cr in c("heuristic", "condition", "osr" )) {
        reas <- e[[ paste0("compile_reason_", cr) ]]
        if (!is.null(reas)) {
          if (nchar(reasAcc) == 0) {
            reasAcc <- paste0(cr, "=", class(reas))
          } else {
            reasAcc <- paste0(reasAcc, ",", cr, "=", class(reas))
          }
        }
      }

      event$reason <- reasAcc

    } else if (cl == "event_compile_end") {
      if (e$succesful) {
        event$type <- "CompilationEnd"
      } else {
        event$type <- "CompilationAborted"
      }

    } else if (cl == "event_compile") {
      event$type <- "Compilation"

      event$ctx <- pp(e$version, "context")

      if (length(e$speculative_contexts) != 0) {
        event$speculative <- format.vector(
          lapply(e$speculative_contexts,
                 function(spec) pp(spec, "speculative"))
        )
      }

      event$bitcode_len <- nchar(e$bitcode)
      event$pir_len <- nchar(e$pir_code)

    } else if (cl == "event_deopt") {
      event$type <- "Deopt"

      event$ctx <- pp(e$version, "context")

      if (e$reason_promise_idx >= 0) {
        mbyPromise <- paste0("Promise ", e$reason_promise_idx)
      } else {
        mbyPromise <- "Baseline"
      }

      reason <- paste0("(", mbyPromise, ",offset=", e$reason_code_off, ")")
      event$reason <- paste0(pp(e$reason, "deopt_reason"), reason)

    } else if (cl == "event_invocation") {
      event$type <- "Invocation"

      event$ctx <- pp(e$context, "context")
      event$call_ctx <- pp(e$callContext, "context")

      event$is_native <- toString(e$isNative)
      event$reason <- pp(e$source, "invocation_source")

      event$callee_address <- pp(e$address, "address")

      event$missing_asmpt <- if (!e$missing_asmpt_present) {
        "NotPresent"
      } else if (!e$missing_asmpt_recovered) {
        "NotRecovered"
      } else {
        "Recovered"
      }

    } else if (cl == "event_unregister_invocation") {
      event$type <- "UnregisterInvocation"
      event$ctx <- pp(e$context, "context")

    } else if (cl == "event_sc") {
      event$type <- "SpeculativeContext"

      event$speculative <- paste0(pp(e$sc, "speculative"), "#", e$index)

      event$is_promise <- toString(e$is_promise)
      event$changed <- toString(e$changed)

    } else {
      event$type <- paste0("[", cl, "]")
    }

    setTxtProgressBar(pb, idx)

    event
  }, mc.cores = cores)

  setTxtProgressBar(pb, length(r$events))

  cat("\n")
  line(columns)
  lapply(events, line)

  endT <- Sys.time()
  cat(" (", difftime(endT, startT, units = "secs"), " secs)", sep="")

  invisible(NULL)
}
