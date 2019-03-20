## Debugging PIR

PIR comes with a variety of options to analyze the output of the compiler in different
stages. To manage the different options we use environment variables. For instance

    PIR_ENABLE=off bin/R -f yourScript.r

completely disables the PIR optimizer. As follows are the different Options available.

### Debug flags

#### Controlling compilation

    PIR_ENABLE=
        on                default, automatically optimize after a number of invocations
        off               disable pir
        force             optimize every function after compiling to rir
        force_dryrun      as above, but throw away the result

    PIR_WARMUP=
        number:            after how many invocations a function is (re-) optimized

#### Debug output options

    PIR_DEBUG=                     (only most important flags listed)
        help                       list all available flags
        PrintIntoStdout            print without buffering (useful for crashes during compilation)
        PrintEarlyRir              print after initial rir2pir translation
        PrintOptimizationPasses    print after every pass
        PrintOptimizationPhases    print before/after every phase of the compiler
        PrintPirAfterOpt           print the fully optimized pir
        PrintFinalPir              print pir after lowering and CSSA conversion
        PrintFinalRir              print rir produced by pir backend

    PIR_DEBUG_PASS_FILTER=
        regex      only show passes matching regex (might need .*)

    PIR_DEBUG_FUNCTION_FILTER=
        regex      only show functions matching regex
                   (might need .*, sometimes names are missing)

    PIR_DEBUG_STYLE=
        Standard   print pir in human-readable format, to view directly in console
        GraphViz   print pir in GraphViz, displaying all instructions within BBs
        GraphVizBB print pir in GraphViz, displaying only BB names and connections
    PIR_MEASURE_COMPILER=
        1          print overal time spend in different passes on shutdown

#### Extended debug flags

    PIR_DEOPT_CHAOS=
        1          randomly trigger some percent of deopts

    PIR_DEBUG_DEOPTS=
        1          show failing assumption when a deopt happens

#### Optimization heuristics

    PIR_INLINER_INITIAL_FUEL=
        n          how many inlinings per inline pass

    PIR_INLINER_MAX_INLINEE_SIZE=
        n          max instruction count for inlinees

    PIR_INLINER_MAX_SIZE=
        n          max instruction count for callers

### Disassembly annotations

#### Assumptions

* `!ExpMi`: Called with no explicitly missing arguments
* `!TMany`: Called with at most the number of required args
* `!TFew` : Called with at least the number of required args
* `EagerN`: Argument `N` is already evaluated
* `!ObjN` : Argument `N` is not an object
* `CooOrd`: Arguments are passed in the correct order (ie. callee reorders)

#### Types Annotations (aka type flags)

* `$` : Is scalar
* `^` : May be lazy (and wrapped in a promise)
* `~` : May be wrapped in a promise (but evaluated)
* `?` : May be missing
* `'` : May not be an object
