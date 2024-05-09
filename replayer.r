recordings.output <- function( r ) {
  if ( is.character(r) ) {
      r <- recordings.load(r)
  }

  get_fun <- function( idx ) {
    f <- r$functions[[ as.integer(idx) + 1 ]]
    return(paste0( f$name, "(", f$env, ")" ))
  }

  for (e in r$events) {
    event <- ""

    if (class(e) == "event_compile") {
      event <- paste("[Compilation]", get_fun(e$closure) )
      event <- paste0( event, "[", recordings.printEventPart( e$dispatch_context, "context" ), "]" )

      # TODO speculative context

      for ( cr in c( "heuristic", "condition", "osr" ) ) {
        reas <- e[[ paste0("compile_reason_", cr) ]]
        if (!is.null(reas)){
          event <- paste0( event, ", ", cr, "=", class(reas) )
        }
      }

    } else if (class(e) == "event_deopt") {
      event <- paste( "[Deopt]", get_fun( e$dispatchTable ) )
      event <- paste0( event, "[", recordings.printEventPart( e$version, "context" ), "]"  )

      event <- paste0( event, ", reason=", recordings.printEventPart( e$reason, "deopt_reason" ) )
      event <- paste0( event, "(", e$reason_code_idx, ")" )
    }

    if (trimws(event) != ""){
      cat( paste0( event, "\n" ) )
    }
  }
}

recordings.csv <- function( r ) {
  if ( is.character(r) ) {
      r <- recordings.load(r)
  }

  result <- ""

  # intersperse a vector with commas
  insert.commas <- function( vec ) {
      acc <- NULL
      flag <- TRUE
      for ( el in vec ){

          if ( flag ){
            flag <- FALSE
          } else {
              acc <- c(acc, "," )
          }

          acc <- c( acc, el )
      }

      return(acc)
  }

  # output a line of vector to result
  line <- function( vec ) {
    vec <- lapply(vec, function(el) {
        if (grepl(",", el)){
            return(paste0( '"', gsub('"', '\\"', el), '"' ))
        } else {
            return(el)
        }
    })
    vec <- insert.commas( vec )
    s <- paste( vec, collapse="" )
    result <<- paste0( result, s, "\n" )
  }

  columns <- c( "type", "fun", "env", "ctx", "reason", "bitcode_len" )

  line( columns )

  get_fun <- function( idx ) {
    f <- r$functions[[ as.integer(idx) + 1 ]]
    return( c(f$name, f$env ) )
  }

  print_ctx <- function(ctx) {
    return( recordings.printEventPart( ctx, "context" ) )
  }

  for (e in r$events) {
    event <- NULL

    if (class(e) == "event_compile") {
      event$type <- "Compilation"

      f <- get_fun( e$closure )
      event$fun <- f[1]
      event$env <- f[2]

      event$ctx <- print_ctx( e$dispatch_context )

      reasAcc <- ""

      for ( cr in c( "heuristic", "condition", "osr" ) ) {
        reas <- e[[ paste0("compile_reason_", cr) ]]
        if (!is.null(reas)){
          if (nchar(reasAcc) == 0) {
            reasAcc <- paste0( cr, "=", class(reas) )
          } else {
            reasAcc <- paste0( reasAcc, ",", cr, "=", class(reas) )
          }
        }
      }

      event$reason <- reasAcc

      event$bitcode_len <- nchar(e$bitcode)

    } else if (class(e) == "event_deopt") {
      event$type <- "Deopt"

      f <- get_fun( e$dispatchTable )
      event$fun <- f[1]
      event$env <- f[2]

      event$ctx <- print_ctx( e$version )
      event$reason <- paste0( recordings.printEventPart( e$reason, "deopt_reason" ), "(", e$reason_code_idx, ")")
    }

    vec <- NULL

    for ( col in columns ){
      if ( is.null( event[[ col ]] ) ){
        vec <- c(vec, "")
      } else {
        vec <- c( vec, event[[ col ]] )
      }
    }

    line( vec )
  }

  return(result)
}
