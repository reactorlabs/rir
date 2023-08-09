# Debugging PIR

## Command Line

PIR comes with a variety of options to analyze the output of the compiler in different
stages. To manage the different options we use environment variables. For instance

    PIR_ENABLE=off bin/R -f yourScript.r

completely disables the PIR optimizer. As follows are the different Options available.

### Debug flags

#### Debug output options

It is possible to dump the intermediate representation of the compiler. By default all the
code is outputted into a directory called compiler-log-[date]. Each function that is compiled
generates a log file and (depending on the configuration) a subdirectory. To produce a
graphical representation of the code choose the GraphViz debug style.

    PIR_DEBUG=                     (only most important flags listed)
        help                       list all available flags
        LLVMDebugInfo              turn on generating debug info for jitted code
        PrintEarlyRir              dump input RIR code
        PrintPirAfterOpt           dump fully optimized PIR code
        PrintFinalPir              dump PIR after lowering and CSSA conversion, right before LLVM conversion
        PrintOptimizationPasses    dump PIR after every pass
        OnlyChanges                for the above, only print if passes change anything
        PrintOptimizationPhases    dump PIR after every phase of the compiler
        OmitDeoptBranches          don't print PIR deopt branches
        PrintInstructionIds        keep a stable PIR instruction id across passes
        PrintPassesIntoFolders     dump PIR into separate files for each pass (always on for GraphViz)
        PrintToStdout              dump logs to stdout instead of files
        PrintUnbuffered            if dumping to stdout, print unbuffered (useful for crashes during compilation)

    PIR_DEBUG_PASS_FILTER=
        regex      only show passes matching regex (might need .*)

    PIR_DEBUG_FUNCTION_FILTER=
        regex      only show functions matching regex
                   (might need .*, sometimes names are missing)

    PIR_DEBUG_STYLE=
        Standard   print pir in human-readable format, to view directly in console
        GraphViz   print pir in GraphViz, displaying all instructions within BBs
        GraphVizBB print pir in GraphViz, displaying only BB names and connections

    RIR_DEBUG_STYLE=
        Standard    print basic information in rir objects in human-readable format
        Detailed    print very detailed information in rir objects, useful for debugging or explaining unexpected semantic differences
        PrettyGraph print in HTML which can be loaded with `tools/rirPrettyGraph` in the same location to display an interactive graph

    PIR_PRINT_INTERNED_RIR_OBJECTS=
        <0|1|path>  if set, folder to print pretty graphs of RIR objects which get interned. If set to 1, prints HTML to stdout. If set to 0 or unset (default), won't print.
                    Interning doesn't occur in normal RIR execution, it will get triggered if RIR_SERIALIZE_CHAOS, PIR_DEBUG_SERIALIZE_LLVM, PIR_CLIENT_ADDR, or PIR_SERVER_ADDR is set.

    PIR_PRINT_INTERNED_RIR_OBJECTS_FREQUENCY=
        n           print pretty graphs of RIR objects which get interned every n-th time, defaults to 10. Otherwise we print a lot more RIR objects than are necessary.

    PIR_LOG_INTERNING=
        1           log every new intern, reused intern, unintern, and other intern related events.

The following flags can be useful for profiling and finding out which passes take how much time to
complete.

    PIR_MEASURING_LOGFILE=
        filename   write the output to filename instead of std::cerr

    PIR_MEASURE_COMPILER=
        1          print overall time spend in different passes on shutdown

    PIR_MEASURE_COMPILER_BACKEND=
        1          print overall time spend in different phases in the backend

    PIR_MEASURE_SERIALIZATION=
        1          print detailed report on time spent in serialization

    PIR_MEASURE_INTERNING=
        1          print detailed report on time spent in interning

    PIR_MEASURE_CLIENT_SERVER=
        1          print time spent in client server communication (sending and receiving requests + processing)

#### Controlling compilation

    PIR_ENABLE=
        on                default, automatically optimize after a number of invocations
        off               disable pir
        force             optimize every function after compiling to rir
        force_dryrun      as above, but throw away the result

    PIR_WARMUP=
        number:            after how many invocations a function is (re-) optimized

#### Extended debug flags

    RIR_CHECK_PIR_TYPES=
        0        Disable
        1        Assert that each PIR instruction conforms to its return type during runtime
        2        Also print out failing instructions (leaks memory and might cause slowdown)

    PIR_DEOPT_CHAOS=
        1          randomly trigger some percent of deopts

    PIR_DEBUG_DEOPTS=
        1          show failing assumption when a deopt happens

    R_DISABLE_GC=
        1          disable the garbage collector

#### Optimization heuristics

For more flags see compiler/parameter.h.

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

    PIR_DEBUG_SERIALIZE_LLVM=
        1          serialize LLVM IR, and add metadata to make it patchable on
                   different sessions. This will be set regardless of the env
                   var if RIR_PRESERVE is set or the compiler server is running,
                   so the only time this is useful is when debugging.

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
* `+` : Not an object but may have attributes
* `_` : Can only have dimension attributes (not an object)
* `-` : No attributes at all

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
* `rir.serialize`: Serializes the SEXP, preserving RIR/PIR-compiled closures, to the given path
* `rir.deserialize`: Deserializes and returns the SEXP at the given path
* `rir.killCompilerServers`: (on client) send a special request to kill compiler servers connected to this client
* `.printInvocation`: prints invocation during evaluation
* `.int3`: breakpoint during evaluation

## PIR & gdb (experimental)

There is some support for debugging LLVM jitted PIR code.
It is enabled by setting the `PIR_DEBUG=LLVMDebugInfo` flag.
It can be useful to set also `PIR_LLVM_OPT_LEVEL=0` to help with preserving debug info through LLVM opt passes.
Optionally, you can specify where the PIR sources for gdb should be stored by setting the environment variable `PIR_GDB_FOLDER`. If not set, a new folder in `/tmp` named `rsh.XXXXXX` is created and these files stored there.
The name of this folder is then written to the file `./PIR_GDB_FOLDER`.
This is handy when you need to set a breakpoint in PIR: you can eg. `ls \`cat PIR_GDB_FOLDER\`` to list the sources.

First, run your program (ideally with `rr`):
```
***@***:~/rir/build/debug$ PIR_DEBUG=LLVMDebugInfo PIR_LLVM_OPT_LEVEL=0 bin/R -e "f <- function(x) { Sys.sleep(0.01); x + 123L }; for (i in 1:4) f(i)" -d rr
rr: Saving execution to trace directory `/home/***/.local/share/rr/R-402'.

R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"
[...]
```

This will produce PIR listing files for each PIR module compiled, named like `f.001` in the folder `/tmp/rsh.XXXXXX`:
```
***@***:~/rir/build/debug$ cat PIR_GDB_FOLDER
/tmp/rsh.FoaeY4
***@***:~/rir/build/debug$ ls `cat PIR_GDB_FOLDER`
f.001
***@***:~/rir/build/debug$ cat -n `cat PIR_GDB_FOLDER`/f.001
     1  rsh_f[0x558eb132cba0].1
     2    <stack bookkeeping>
     3  BB0
     4    val^            %0.0  = LdArg                    0
     5    val?^           %0.1  = LdVar              eR    Sys.sleep, GlobalEnv
     6    cls$⁻           %0.2  = LdConst                  function (time)
     7    lgl$#⁻          %0.3  = Identical                %0.1, %0.2
     8    void                    Branch                   %0.3 -> BB10 (if true) | BB11 (if false)
     9
    10  BB10   <- [0]
    11    real$#⁻         %10.0 = LdConst                  0.01
    12    code⁻           %10.1 = LdConst                  Sys.sleep(0.01)
    13    env             e10.2 = (MkEnv)            l     x=%0.0, parent=GlobalEnv, context 1
    14    ct              %10.3 = PushContext        lCL   %10.0, %10.1, %0.2, e10.2
    15    env             e10.4 = MKEnv              l     time=%10.0, parent=BaseNamespace, context 1
    16    val?            %10.5 = CallBuiltin        !v    Sys.sleep(%10.0) e10.4
    17    lgl$#⁻          %10.6 = IsEnvStub          R     , e10.2
    18    void                    Branch                   %10.6 -> BB12 (if true) | BB13 (if false)
    19
    20  BB12   <- [10]
    21    val?            %12.0 = PopContext         C     %10.5, %10.3
    22    lgl$#⁻          %12.1 = IsEnvStub          R     , e10.2
    23    void                    Branch                   %12.1 -> BB14 (if true) | BB15 (if false)
    24
    25  BB14   <- [12]
    26    val?<int$⁻>     %14.0 = Force!<lazy>       !vrL  %0.0,
    27    lgl$#⁻          %14.1 = IsType                   %14.0 isA int$⁻
    28    void                    Branch                   %14.1 -> BB16 (if true) | BB17 (if false)
    29
    30  BB16   <- [14]
    31    int$⁻           %16.0 = CastType           d     dn %14.0
    32    lgl$#⁻          %16.1 = IsEnvStub          R     , e10.2
    33    void                    Branch                   %16.1 -> BB18 (if true) | BB19 (if false)
    34
    35  BB11   <- [0]
    36    void                    RecordDeoptReason  m     %0.1
    37    env             e11.1 = MKEnv              l     x=%0.0, parent=GlobalEnv, context 1
    38    void                    ScheduledDeopt           0x558eb0d67bf0+0: [], env=e11.1
    39
    40  BB13   <- [10]
    41    void                    RecordDeoptReason  m     e10.2
    42    void                    ScheduledDeopt           0x558eb0d67bf0+49: [], env=e10.2; 0x558eb1acc118+27: [%10.5], env=e10.4
    43
    44  BB15   <- [12]
    45    void                    RecordDeoptReason  m     e10.2
    46    void                    ScheduledDeopt           0x558eb0d67bf0+49: [%12.0], env=e10.2
    47
    48  BB17   <- [14]
    49    void                    RecordDeoptReason  m     %14.0
    50    goto BB7
    51  BB7   <- [17, 19]
    52    void                    ScheduledDeopt           0x558eb0d67bf0+59: [%14.0], env=e10.2
    53
    54  BB18   <- [16]
    55    int$#⁻          %18.0 = LdConst                  123
    56    int$⁻<>         %18.1 = Add                vd    %16.0, %18.0, elided
    57    void                    Return                   %18.1
    58
    59  BB19   <- [16]
    60    void                    RecordDeoptReason  m     e10.2
    61    goto BB7
    62
```

Now you can run the debugger and set breakpoints in those files:
```
***@***:~/rir/build/debug$ rr replay
GNU gdb (Ubuntu 8.1.1-0ubuntu1) 8.1.1
[...]
0x00007f53e481d090 in _start () from /lib64/ld-linux-x86-64.so.2
(rr) hbreak f.001:1
No source file named f.001.
Make hw breakpoint pending on future shared library load? (y or [n]) y
Hardware assisted breakpoint 1 (f.001:1) pending.
(rr) hbreak f.001:56
No source file named f.001.
Make hw breakpoint pending on future shared library load? (y or [n]) y
Hardware assisted breakpoint 2 (f.001:56) pending.
(rr) c
Continuing.

R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"
[...]

Breakpoint 1, rsh_f[0x558eb132cba0].1 () at f.001:1
1       rsh_f[0x558eb132cba0].1
(rr) info locals
pir_0_0 = <optimized out>
pir_0_1 = <optimized out>
[...]
(rr) n
2         <stack bookkeeping>
(rr) n
4         val^            %0.0  = LdArg                    0
(rr) n
5         val?^           %0.1  = LdVar              eR    Sys.sleep, GlobalEnv
(rr) n
7         lgl$#⁻          %0.3  = Identical                %0.1, %0.2
(rr) p pir_0_1
$1 = (SEXP) 0x558eb0582c50
(rr) ptype pir_0_1
type = struct SEXPREC {
    struct sxpinfo_struct sxpinfo;
    struct SEXPREC *attrib;
    struct SEXPREC *gengc_next_node;
    struct SEXPREC *gengc_prev_node;
    union {
        struct primsxp_struct primsxp;
        struct symsxp_struct symsxp;
        struct listsxp_struct listsxp;
        struct envsxp_struct envsxp;
        struct closxp_struct closxp;
        struct promsxp_struct promsxp;
    } u;
} *
(rr) p *pir_0_1
$2 = {sxpinfo = {type = 5, scalar = 0, obj = 0, alt = 0, gp = 0, mark = 1,
    debug = 0, trace = 0, spare = 0, gcgen = 0, gccls = 0, named = 7,
    extra = 0}, attrib = 0x558eb04cb240, gengc_next_node = 0x558eb059a1d0,
  gengc_prev_node = 0x558eb04cac20, u = {primsxp = {offset = -1304651768},
    symsxp = {pname = 0x558eb23c9808, value = 0x558eb0582c88,
      internal = 0x558eb04cb240}, listsxp = {carval = 0x558eb23c9808,
      cdrval = 0x558eb0582c88, tagval = 0x558eb04cb240}, envsxp = {
      frame = 0x558eb23c9808, enclos = 0x558eb0582c88,
      hashtab = 0x558eb04cb240}, closxp = {formals = 0x558eb23c9808,
      body = 0x558eb0582c88, env = 0x558eb04cb240}, promsxp = {
      value = 0x558eb23c9808, expr = 0x558eb0582c88, env = 0x558eb04cb240}}}
(rr) p Rf_PrintValue(pir_0_1)
<promise: 0x558eb0582c50>
$3 = void
(rr) c
Continuing.

Breakpoint 2, rsh_f[0x558eb132cba0].1 () at f.001:56
56        int$⁻<>         %18.1 = Add                vd    %16.0, %18.0, elided
(rr) p pir_16_0
$4 = 3
(rr) ptype pir_16_0
type = int
(rr) p pir_18_1
$5 = <optimized out>
(rr) n
57        void                    Return                   %18.1
(rr) p pir_18_1
$6 = 126
(rr) n
2         <stack bookkeeping>
(rr) n
57        void                    Return                   %18.1
(rr) n
rir::evalRirCode (c=0x558eb1acc7f8, ctx=0x558eb086d000, env=0x558eb08748c8,
    callCtxt=0x7ffd5ef08030, initialPC=0x0, cache=0x0)
    at ../../rir/src/interpreter/interp.cpp:1935
1935                                 env, callCtxt ? callCtxt->callee : nullptr);
(rr) rn

Breakpoint 2, rsh_f[0x558eb132cba0].1 () at f.001:56
56        int$⁻<>         %18.1 = Add                vd    %16.0, %18.0, elided
(rr) bt
#0  rsh_f[0x558eb132cba0].1 () at f.001:56
#1  0x00007f53e0320c6d in rir::evalRirCode (c=0x558eb1acc7f8,
    ctx=0x558eb086d000, env=0x558eb08748c8, callCtxt=0x7ffd5ef08030,
    initialPC=0x0, cache=0x0) at ../../rir/src/interpreter/interp.cpp:1934
#2  0x00007f53e0331b1c in rir::evalRirCode (c=0x558eb1acc7f8,
    ctx=0x558eb086d000, env=0x558eb08748c8, callCtxt=0x7ffd5ef08030)
    at ../../rir/src/interpreter/interp.cpp:3798
#3  0x00007f53e031d0d7 in rir::rirCallTrampoline_ (cntxt=..., call=...,
    code=0x558eb1acc7f8, env=0x558eb08748c8, ctx=0x558eb086d000)
    at ../../rir/src/interpreter/interp.cpp:502
[...]
```

Current limitations:
* ~~`continue` after setting a breakpoint breaks something in `rr` and kills the session~~ Use `hbreak` instead of `break` (creates a hw breakpoint, see [https://github.com/rr-debugger/rr/issues/2163#issuecomment-363230091](https://github.com/rr-debugger/rr/issues/2163#issuecomment-363230091))
* `reverse-continue` not working, reports a couple warnings `warning: Corrupted shared library list: 0x558eb0cccca0 != 0x7f53e49fd5b0` and `warning: Temporarily disabling breakpoints for unloaded shared library "/home/jecmejan/rir/build/debug/librir.so"` and runs all the way to the start of the program
* ~~No variable information (can't do `p %0.1`)~~ Works at places (when the values are not optimized out) by `p pir_0_1` (for printing PIR variable `%0.1`). Also useful can be `info locals`
* Stepping into functions - goes somewhere to the calling convention guts but then when stepping to the actual native call it behaves as next without a breakpoint in the callee :(

## PIR and perf (experimental)

To get support for `perf` profiling:
* ~~Build LLVM from source with `perf` support enabled (eg., pass `-DLLVM_USE_PERF:BOOL=ON` to `cmake`, see `sync.sh` for details, don't forget to set the LLVM symlink in `external`)~~ _This should be the default now_
* Pass `-DLLVM_USE_PERF=1` to Ř `cmake`
* Record: `PIR_DEBUG=LLVMDebugInfo perf record -k 1 bin/R -f test.r`
* Inject jit info: `perf inject -j -i perf.data -o perf.data.jitted`
* Browse: `perf report -i perf.data.jitted`

Sample:
```
Samples: 12K of event 'cycles', Event count (approx.): 11323436187
Overhead  Command   Shared Object         Symbol
   4.17%  R         R                     [.] Rf_allocVector3
   2.75%  R         libc-2.27.so          [.] malloc
   1.32%  R         librir.so             [.] rir::pir::ForcedBy::merge
   1.08%  R         R                     [.] do_subset_dflt
   ...
   0.46%  R         jitted-37428-112.so   [.] rsh_advance[0x558e2d448380].5
   ...
```

See [https://lists.llvm.org/pipermail/llvm-dev/2019-January/129160.html](https://lists.llvm.org/pipermail/llvm-dev/2019-January/129160.html)

# Debug Ř using rr inside Docker

In order to use rr inside a docker container, it is necessary to run it with some security capabilites:

`docker run --cap-add=SYS_PTRACE --security-opt seccomp=unconfined -it registry.gitlab.com/rirvm/rir_mirror/benchmark:SOME_COMMIT_ID`

Recording Ř works just fine, with the usual `-d rr` . However, when running  `rr replay`, it complains about not being able to find the debug symbols. To overcome this issue type in: `/opt/rir/external/custom-r/bin/exec/R` right after `rr replay` (within the *rr* prompt).

## CLion

You can create run/debug configurations for Ř in CLion.

CLion should be smart enough to automatically generate a CMake configuration from our `CMakeLists.txt`, and thus have some preset run configurations. The one you will use is `rir`. This configuration will already build Ř in a folder called `cmake-build-debug`. However, you must do some manual configuration in order to get it to run properly:

- Change `executable` to `<path to rir>/external/custom-r/bin/exec/R` (this is the path to the R executable that was built by Ř; replace `<path to rir>` with the actual repo path)
- Set the following environment variables (again, replace `<path to rir>` with the actual repo path):
  - On Linux: `LD_LIBRARY_PATH=<path to rir>/external/custom-r/lib;EXTRA_LOAD_R=<path to rir>/rir/R/rir.R;EXTRA_LOAD_SO=<path to rir>/cmake-build-sanitize/Debug/librir.dylib;R_DOC_DIR=<path to rir>/external/custom-r/doc;R_HOME=<path to rir>/external/custom-r;R_HOME_DIR=<path to rir>/external/custom-r;R_INCLUDE_DIR=<path to rir>/external/custom-r/include;R_SHARE_DIR=<path to rir>/external/custom-r/share`
  - On macOS: `DYLD_LIBRARY_PATH=<path to rir>/external/custom-r/lib;EXTRA_LOAD_R=<path to rir>/rir/R/rir.R;EXTRA_LOAD_SO=<path to rir>/cmake-build-sanitize/Debug/librir.dylib;R_DOC_DIR=<path to rir>/external/custom-r/doc;R_HOME=<path to rir>/external/custom-r;R_HOME_DIR=<path to rir>/external/custom-r;R_INCLUDE_DIR=<path to rir>/external/custom-r/include;R_SHARE_DIR=<path to rir>/external/custom-r/share`

This should be enough to get run to start the REPL, and debug to work with breakpoints. You can also redirect input from files (e.g. one of the tests), or add extra environment variables like `PIR_DEBUG`.