recordings.disassemble <- function( r ){
    if ( is.character(r) ){
        r <- recordings.load(r)
    }

    funs <- NULL
    for (f in r$functions) {
        fun <- list( name = f$name, env = f$env, compilations = NULL, deopts = NULL )
        funs <- c(funs, list(fun))
    }

    i <- 0
    for (in_event in r$events) {
        event <- list( idx = i )
        if (class(in_event) == "event_compile") {
            event$dispatch_ctx <- in_event$dispatch_context

            for (sc in in_event$speculative_contexts){
                event$speculative <- c(event$speculative, sc)
            }

            for ( cr in c( "heuristic", "condition", "osr" ) ){
                event[[cr]] <- in_event[[ paste0("compile_reason_", cr) ]]
            }

            clos <- as.integer(in_event$closure) + 1
            funs[[clos]]$compilations <- c(funs[[clos]]$compilations, list(event))
        } else if (class(in_event) == "event_deopt") {
            event$version <- in_event$version
            event$reason <- in_event$reason
            event$offset <- in_event$reason_code_idx

            clos <- as.integer(in_event$dispatchTable) + 1
            funs[[clos]]$deopts <- c(funs[[clos]]$deopts, list(event))
        }
        i <- i + 1
    }

    return(funs)
}

recordings.prettyPrint <- function( funs ) {
    pp <- recordings.printEventPart
    line <- function(...) cat( paste0( ..., "\n"))

    for ( fun in funs ){
        line( "====", fun$name, "(", fun$env, ")", "====")
        line("Compilations:")

        for ( e in fun$compilations ){
            line( " ", e$idx, ":" )

            line("  ctx=", pp( e$dispatch_ctx, "context" ))

            line("  compile_reason_heuristic=", pp( e$heuristic, "reason" ))
            line("  compile_reason_condition=", pp( e$condition, "reason" ))
            line("  compile_reason_osr=", pp( e$osr, "reason" ))
        }

        line("Deopts:")

        for ( e in fun$deopts ){
            line( " ", e$idx, ":" )

            line("  ver=", pp( e$version, "context" ))
            line("  reason=", pp( e$reason, "deopt_reason" ), "(" , e$offset, ")")
        }
    }
}
