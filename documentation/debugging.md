# Debugging PIR

## Command Line

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
        PrintPassesIntoFolders     print each pass into `log/<closure>-pir-function/<pass#>.log` (or `.out` if GraphViz)
        PrintIntoFiles             print into `log/<closure>-pir-function.log` (or `.out` if GraphViz)
        PrintIntoStdout            print without buffering (useful for crashes during compilation)
        PrintInstructionIds        have instructions print out their memory addresses, to track them across compilation passes
        OmitDeoptBranches          don't print deopt branches in closures
        OnlyChanges                only print optimization passes/phases which actually change bytecode
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

    PIR_MEASURING=
        1          enable event and time measuring

    PIR_MEASURING_LOGFILE=
        filename   write the output to filename instead of std::cerr

    PIR_MEASURE_COMPILER=
        1          print overal time spend in different passes on shutdown

    RIR_CHECK_PIR_TYPES=
        0        Disable
        1        Assert that each PIR instruction conforms to its return type during runtime
        2        Also print out failing instructions (leaks memory and might cause slowdown)

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

#### Serialize flgas

    RIR_PRESERVE=
        1          serialize RIR closures on exit. NOTE: will deserialize a
                   compiled closure from a prior session even if this is off

    RIR_SERIALIZE_CHAOS=
        n          serialize and deserialize the dispatch table on every `n`th
                   RIR call. WARNING: This sometimes prevents optimization

### Disassembly annotations

#### Assumptions

* `!ExpMi`: Called with no explicitly missing arguments
* `!TMany`: Called with at most the number of required args
* `!TFew` : Called with at least the number of required args
* `EagerN`: Argument `N` is already evaluated
* `!ObjN` : Argument `N` is not an object
* `CooOrd`: Arguments are passed in the correct order (ie. callee reorders)

#### Type Annotations (aka type flags)

* `$` : Is scalar
* `#` : Is never NA or NaN
* `^` : May be lazy (and wrapped in a promise)
* `~` : May be wrapped in a promise (but evaluated)
* `?` : May be missing
* `'` : Not an object but may have attributes
* `"` : Not an object and has no attributes

#### Effects

* `v` : Visibility
* `w` : Warn
* `e` : Error
* `f` : Force
* `r` : Reflection
* `l` : LeakArg
* `C` : ChangesContexts
* `R` : ReadsEnv
* `W` : WritesEnv
* `L` : LeaksEnv
* `D` : TriggerDeopt
* `X` : ExecuteCode
* `d` : DependsOnAssume

`!` means that an instruction has all effects *except* the following. e.g. `!r`
means an instruction has all effects but reflection, just `!` means it has all
effects.

## Within R

PIR also adds some functions to the global environment, for the sole purpose of
debugging:

* `rir.markOptimize`: Tells the compiler to optimize the function
* `rir.isValidFunction`: Returns TRUE if the argument is a rir-compiled closure
* `rir.disassemble`: prints the disassembled rir function
* `rir.printInvocation`: prints how many times the (optimized) rir function was
  called
* `rir.compile`: compiles the given closure or expression, returns the compiled
  version
* `pir.compile`: expects a rir-compiled closure, optimizes it
* `pir.tests`: runs some internal regression tests
* `pir.check`: returns TRUE if f, when PIR compiled, satisfies the given checks.
* `pir.debugFlags`: creates a bitset with pir debug options
* `pir.setDebugFlags`: sets the default debug options for pir compiler
* `rir.compile.program`: compiles code of the given file all in a function, and
  returns the functon
* `rir.eval`: evaluates the code in RIR
* `rir.body`: returns the body of rir-compiled function. The body is the vector
  containing its ast maps and code objects
* `.printInvocation`: prints invocation during evaluation
* `.int3`: breakpoint during evaluation

## PIR & GDB (experimental!)

There is *WIP* support for debugging LLVM jitted PIR code. It is enabled by `#define PIR_GDB_SUPPORT` in `compiler/native/pir_debug_info.h`, currently set for debug builds.

Run your program, ideally with `rr`:
```
***@***:~/rir/build/debug$ PIR_GDB_FOLDER=pirgdb bin/R -e "f <- function(x) { Sys.sleep(0.01); x + 123L }; f(4L); f(4L); f(4L); f(4L)" -d rr
rr: Saving execution to trace directory `/home/***/.local/share/rr/R-402'.

R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"
[...]
```

This will produce PIR listing files for each PIR module compiled, named like `f.001` in the folder specified by `PIR_GDB_FOLDER` (defaults to `pirgdb`, this folder will be wiped if it already exists):
```
***@***:~/rir/build/debug$ cat pirgdb/f.001
rsh_f[0x56500a98aa80].1
BB0
  int$~"          %0.0  = LdArg                    0
  val?^           %0.1  = LdVar              eR    Sys.sleep, GlobalEnv
  cls"            %0.2  = LdConst                  function (time)
  lgl$#"          %0.3  = Identical                %0.1, %0.2
  void                    Branch                   %0.3 -> BB4 (if true) | BB5 (if false)
BB4   <- [0]
  real$#"         %4.0  = LdConst                  0.01
  prom"           %4.1  = MkArg                    %4.0, Prom(0) (!refl),
  code"           %4.2  = LdConst                  Sys.sleep(0.01)
  env"            e4.3  = (MkEnv)            l     x=%0.0, parent=GlobalEnv, context 1
  ct              %4.4  = PushContext        lCL   %4.1, %4.2, %0.2, e4.3
  env"            e4.5  = MKEnv              l     time=%4.0, parent=BaseNamespace, context 1
  val?            %4.6  = CallBuiltin        !v    Sys.sleep(%4.0) e4.5
  val?            %4.7  = PopContext         C     %4.6, %4.4
  int$"<>         %4.8  = Force!<lazy>       d     %0.0,
  int$#"          %4.9  = LdConst                  123
  int$"<>         %4.10 = Add                vd    %4.8, %4.9, elided
  void                    Return                   %4.10
BB5   <- [0]
  void                    RecordDeoptReason  m     %0.1
  env"            e5.1  = MKEnv              l     x=%0.0, parent=GlobalEnv, context 1
  void                    ScheduledDeopt           0x5650095589a0+0: [], env=e5.1

rsh_f[0x56500a98aa80]_Prom(0).1
BB0
  real$#"         %0.0  = LdConst                  0.01
  void                    Visible            v
  void                    Return                   %0.0
```

Now, you can run the debugger and set a breakpoint in those files:
```
***@***:~/rir/build/debug$ rr replay
GNU gdb (Ubuntu 8.1.1-0ubuntu1) 8.1.1
[...]
0x00007f0bdeeef090 in _start () from /lib64/ld-linux-x86-64.so.2
(rr) b f.001:3
No source file named f.001.
Make breakpoint pending on future shared library load? (y or [n]) y
Breakpoint 1 (f.001:3) pending.
(rr) c
Continuing.

R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"
[...]

Breakpoint 1, rsh_f[0x56500a98aa80].1 () at f.001:3
3         int$~"          %0.0  = LdArg                    0
(rr) n
4         val?^           %0.1  = LdVar              eR    Sys.sleep, GlobalEnv
(rr) n
6         lgl$#"          %0.3  = Identical                %0.1, %0.2
(rr) n
10        prom"           %4.1  = MkArg                    %4.0, Prom(0) (!refl),
(rr) n
12        env"            e4.3  = (MkEnv)            l     x=%0.0, parent=GlobalEnv, context 1
(rr) bt
#0  rsh_f[0x56500a98aa80].1 () at f.001:12
#1  0x00007f0bd9a5a223 in rir::evalRirCode (c=0x56500aa2fcd8, ctx=0x5650097ce110,
    env=0x5650097d59a8, callCtxt=0x7fff4d1290d0, initialPC=0x0, cache=0x0)
    at ../../rir/src/interpreter/interp.cpp:1918
#2  0x00007f0bd9a6b0ee in rir::evalRirCode (c=0x56500aa2fcd8, ctx=0x5650097ce110,
    env=0x5650097d59a8, callCtxt=0x7fff4d1290d0)
    at ../../rir/src/interpreter/interp.cpp:3783
#3  0x00007f0bd9a5676e in rir::rirCallTrampoline_ (cntxt=..., call=...,
    code=0x56500aa2fcd8, env=0x5650097d59a8, ctx=0x5650097ce110)
    at ../../rir/src/interpreter/interp.cpp:501
#4  0x00007f0bd9a568f9 in rir::rirCallTrampoline (call=..., fun=0x56500aa2ff98,
    env=0x5650097d59a8, arglist=0x56500b32ffc0, ctx=0x5650097ce110)
    at ../../rir/src/interpreter/interp.cpp:529
#5  0x00007f0bd9a6ce41 in rir::rirCallTrampoline (call=..., fun=0x56500aa2ff98,
    arglist=0x56500b32ffc0, ctx=0x5650097ce110)
    at ../../rir/src/interpreter/interp.cpp:545
#6  0x00007f0bd9a6d34a in rir::rirCall (call=..., ctx=0x5650097ce110)
    at ../../rir/src/interpreter/interp.cpp:1054
#7  0x00007f0bd9a6b56e in rir::rirApplyClosure (ast=0x56500b330068,
    op=0x56500b328cb8, arglist=0x56500b32ffc0, rho=0x56500945e800,
    suppliedvars=0x56500942c190) at ../../rir/src/interpreter/interp.cpp:3827
#8  0x00005650070d9352 in Rf_applyClosure (call=call@entry=0x56500b330068,
    op=op@entry=0x56500b328cb8, arglist=0x56500b32ffc0,
    rho=rho@entry=0x56500945e800, suppliedvars=<optimized out>) at eval.c:1704
#9  0x00005650070d6c15 in Rf_eval (e=e@entry=0x56500b330068,
    rho=rho@entry=0x56500945e800) at eval.c:775
#10 0x0000565007105dbd in Rf_ReplIteration (rho=0x56500945e800, savestack=0,
    browselevel=0, state=0x7fff4d129400) at main.c:260
#11 0x0000565007106181 in R_ReplConsole (rho=0x56500945e800, savestack=0,
    browselevel=0) at main.c:310
#12 0x0000565007106232 in run_Rmainloop () at main.c:1086
#13 0x0000565007106282 in Rf_mainloop () at main.c:1093
#14 0x000056500700cc98 in main (ac=ac@entry=3, av=av@entry=0x7fff4d12a548)
    at Rmain.c:29
#15 0x00007f0bdd1ebbf7 in __libc_start_main (main=0x56500700cc80 <main>, argc=3,
    argv=0x7fff4d12a548, init=<optimized out>, fini=<optimized out>,
    rtld_fini=<optimized out>, stack_end=0x7fff4d12a538)
    at ../csu/libc-start.c:310
#16 0x000056500700ccca in _start ()
(rr)
```

Current limitations:
* `continue` after setting a breakpoint breaks something in `rr` and kills the session
* no variable information (can't do `p %0.1`)
* sort of stepping into functions but not tested much
