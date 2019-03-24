# RIR

## Overview

RIR is a bytecode, similar to the JVM bytecode. It stands for "R Intermediate Representation".

R is an "interpreted" language, in that every statement of code is executed immediately when it's parsed. However, re-parsing every statement is slow. So, when the RIR interpreter parses code, it first compiles it into "RIR bytecode", *then* immediately executes the bytecode. When a call to previously-parsed code is encountered, like in a loop or function call, instead of re-parsing the code, RIR will simply run the existing bytecode. This is called JIT ("Just in Time") compilation.

### What RIR bytecode looks like

To see a function's bytecode, run `./bin/R`, then type `f <- rir.compile(<function>)`, then `rir.disassemble(f)`. Here's an example:

```text
> f <- rir.compile(function() {
+   x <- 10
+   y <- 1
+   while (y < x) {
+     y <- y * 2
+   }
+   y
+ })
> rir.disassemble(f)
* closure 0x55c312b5a8b8 (vtable 0x55c312446a38, env 0x55c310d3f4b8)
= vtable slot <0> (0x55c312446988, invoked 0) =
# needsEnv
0:
      0   guard_fun_  { == 0x55c310d154f0
     13   guard_fun_  <- == 0x55c310d0a478
     26   push_  [1] 10
     31   set_shared_
     32   dup_
     33   stvar_  x
     38   invisible_
     39   pop_
     40   guard_fun_  <- == 0x55c310d0a478
     53   push_  [1] 1
     58   set_shared_
     59   dup_
     60   stvar_  y
     65   invisible_
     66   pop_
     67   guard_fun_  while == 0x55c310d0a910
1:
     80   guard_fun_  < == 0x55c310d16ca0
     93   ldvar_  y
     98   ldvar_  x
    103   [ <?> x <?> ]
    112   ; y < x
          lt_
    113   asbool_
    114   brfalse_  2
    119   guard_fun_  { == 0x55c310d154f0
    132   guard_fun_  <- == 0x55c310d0a478
    145   guard_fun_  * == 0x55c310d171e0
    158   ldvar_  y
    163   push_  [1] 2
    168   [ <?> x <?> ]
    177   ; y * 2
          mul_
    178   set_shared_
    179   dup_
    180   stvar_  y
    185   invisible_
    186   pop_
    187   br_  1
2:
    192   push_  NULL
    197   invisible_
    198   pop_
    199   ldvar_  y
    204   ret_
```

This contains some instructions, like `guard_fun_` and `invisible_`, which you can ignore for now. Here is roughly the same bytecode as above, but simpler, side-by-side with the unparsed code:

```r
function() {      |
  x <- 10         | 0:  push 10; stvar x; pop;
  y <- 1          |     push 1; stvar y; pop;
  while (y < x) { | 1:  ldvar y; ldvar x; lt; brfalse 2;
    y <- y * 2    |     ldvar y; push 2; mul; stvar y; pop;
  }               |     goto 1
  y               | 2:  ldvar y; ret
}                 |
```

## Controlling execution

    RIR_PROFILING=
        on                default, profiles every call and a bunch of operations so that an optimizer could eventually leverage on the run-time information
        off               disable profiling

## Comparison to GNU-R

The default R interpreter (GNU-R) is also a JIT compiler with a bytecode. The main difference between this bytecode and RIR is that GNU-R has a few "fat" instructions, which are more complicated, while RIR has many more instructions, but they're simpler. For example, RIR has explicit instructions for creating environments, but GNU-R doesn't.

RIR actually relies on some of GNU-R, such as the parser. RIR basically "hacks" GNU-R so that it evaluates functions using RIR instead. Even when interpreting, we still rely on GNU-R for some things - functions which are prefixed with `R_` or `Rf_` are GNU-R functions.

## Code Structure

Here are a few important files and functions:

- [`R/rir.R`](../rir/R/rir.R): Defines extra functions in R which let you access RIR/PIR, like `rir.compile` and `pir.compile`. This is run whenever RIR R is loaded.
- [`api.cpp`](../rir/src/api.cpp): Allows us to "inject" RIR into R, reads environment variables and debug flags, and provides the implementation for the functions in `rir.R`.
  - [`rir_compile`](../rir/src/api.cpp#L45): `rir.compile` - this "hacks" a function so that it gets evaluated in RIR.
- [`Compiler.cpp`](../rir/src/Compiler.cpp): Converts the parsed R AST into RIR (GNU-R parses the AST).
  - [`compileSpecialCall`](../rir/src/ir/Compiler.cpp#L137): Converts calls to certain, "special" functions (e.g. `+`, `for`) into their own bytecodes. This way we can interpret them faster.
- [`interp.cpp`](../rir/src/interpreter/interp.cpp): Contains almost the entire RIR interpreter, that's why it's so huge.
  - [`evalRirCode`](../rir/src/interpreter/interp.cpp#L1287): The specific place where each bytecode is interpreted. This is one big loop 1) for simplicity and 2) more importantly, because all of the code here needs to be very fast.
- [`rir_2_pir` - `Rir2Pir::compileBC`](../rir/src/compiler/translations/rir_2_pir/rir_2_pir.cpp#L173): Converts RIR into PIR.
- [`pir_2_rir` - `Pir2Rir::compileCode`](../rir/src/compiler/translations/pir_2_rir/pir_2_rir.cpp#L509): Converts PIR into RIR.

## What about PIR?

PIR is another representation of R. However, PIR is *not* a bytecode, and PIR code isn't interpreted - instead, it's designed to be optimized. While interpreting, the compiler converts RIR code into PIR, then performs some optimization passes, then converts the PIR back into RIR - *assuming the compiler works*, the new RIR code is semantically the same as the old, but faster. The RIR interpreter also performs some simple optimizations directly when compiling R into RIR.

For more information about PIR, see [pir.md](./pir.md)
