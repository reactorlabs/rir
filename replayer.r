recordings.csv <- function(r, out = "") {
  if (is.character(r)) {
      r <- recordings.load(r)
  }

  if (out != "" && file.exists(out)){
    file.remove(out)
  }

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

  # output a line of vector
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
    vec <- paste0(vec, "\n")
    cat( vec, file = out, append = TRUE )
  }

  columns <- c("idx", "type", "fun", "env", "ctx", "speculative_ctx", "speculative", "call_ctx", "reason", "bitcode_len", "pir_len", "changed", "is_promise", "is_native", "callee_address")

  line(columns)

  pp <- function(obj, type) {
    recordings.printEventPart(obj, type, r$functions)
  }

  idx <- 1

  for (e in r$events) {
    event <- NULL

    event$idx <- idx
    idx <- idx + 1

    f <- r$functions[[ as.integer(e$funIdx) + 1 ]]
    event$fun <- f$name
    event$env <- f$env

    if (class(e) == "event_compile_start") {
      event$type <- "CompilationStart"

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

    } else if (class(e) == "event_compile_end") {
      if (e$succesful){
          event$type <- "CompilationEnd"
      } else {
          event$type <- "CompilationAborted"
      }

    } else if (class(e) == "event_compile") {
      event$type <- "Compilation"

      event$ctx <- pp(e$version, "context")

      event$speculative <- paste(insert.commas(lapply(
        e$speculative_contexts,
        function(spec) pp(spec, "speculative")
      )), collapse="")

      event$bitcode_len <- nchar(e$bitcode)
      event$pir_len <- nchar(e$pir_code)

    } else if (class(e) == "event_deopt") {
      event$type <- "Deopt"

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

      event$ctx <- pp(e$context, "context")

      event$call_ctx <- pp(e$callContext, "context")
      event$is_native <- e$isNative
      event$reason <- pp(e$source, "invocation_source")

      event$callee_address <- pp(e$address, "address")

    } else if (class(e) == "event_unregister_invocation"){
      event$type <- "UnregisterInvocation"

    } else if (class(e) == "event_sc") {
      event$type <- "SpeculativeContext"

      event$speculative <- paste0(pp( e$sc, "speculative" ), "@", e$index)

      event$is_promise <- e$is_promise
      event$changed <- e$changed
    } else {
      event$type <- paste0("[", class(e), "]")
    }

    vec <- lapply(columns, function(col) {
        if (is.null(event[[ col ]]))
            ""
        else
            event[[ col ]]
    })

    line(vec)
  }
}
