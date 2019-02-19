# RIR

## Overview

RIR is a bytecode, similar to the JVM bytecode. It stands for "R Intermediate Representation".

R is an "interpreted" language, in that every statement of code is executed immediately when it's parsed. However, re-parsing every statement is slow. So, when the RIR interpreter parses code, it first compiles it into "RIR bytecode", *then* immediately executes the bytecode. When a call to previously-parsed code is encountered, like in a loop or function call, instead of re-parsing the code, RIR will simply run the existing bytecode. This is called JIT ("Just in Time") compilation.

### What RIR bytecode looks like

To roughly see a function's bytecode, run `PIR_DEBUG=PrintPirAfterOpt ./bin/R`, then type `f <- rir.compile(<function>)`, then `pir.compile(f)`. Here's an example:

<pre>
&gt; f &lt;- rir.compile(function() {
+   x &lt;- 10
+   y &lt;- 1
+   while (y &lt; x) {
+     y &lt;- y * 2
+   }
+   y
+ })
&gt; pir.compile(f)

<font color="#295FCC"><b>╞═══════════════════════════════╡  </b></font>Compiling f<font color="#295FCC"><b>  ╞══════════════════════════════╡</b></font>

<font color="#FF8787"><b>┌──────────────────────────────────────────────────────────────────────────────┐</b></font>
<font color="#FF8787"><b>│ f[0x55d05c948220]                                                            │</b></font>
<font color="#FF8787"><b>│ Assumptions: CorrOrd, !TMany                                                 │</b></font>
<font color="#FF8787"><b>│ Properties:  Eager, !Reflection                                              │</b></font>
<font color="#FF8787"><b>├────── PIR Version After Optimizations</b></font>
f[0x55d05c948220]
BB0
  void            Invisible
  real$&apos;  %0.1  = LdConst          [1] 1
  void            Invisible
  goto BB1
BB1
  val?&apos;   %1.0  = Phi              %0.1:BB0, %3.1:BB3
  val?&apos;   %1.1  = Phi              %0.1:BB0, %3.1:BB3
  val?&apos;   %1.2  = Phi              %0.1:BB0, %3.1:BB3
  real$&apos;  %1.3  = LdConst          [1] 10
  lgl&apos;    %1.4  = Lt               %1.2, %1.3, elided
  t       %1.5  = AsTest           %1.4
  void            Branch           %1.5 -&gt; BB3 (if true) | BB2 (if false)
BB3
  real$&apos;  %3.0  = LdConst          [1] 2
  val?&apos;   %3.1  = Mul              %1.0, %3.0, elided
  void            Invisible
  goto BB1
BB2
  void            Invisible
  void            Return           %1.1

<font color="#FF8787"><b>│ f[0x55d05c948220]                                                            │</b></font>
<font color="#FF8787"><b>└──────────────────────────────────────────────────────────────────────────────┘</b></font>
</pre>

Note that these are actually PIR instructions, not RIR bytecode instructions, so they contain extra information (like `Phi`s). They also contain assumptions and `Invisible` statements, which have little effect, you can just ignore them for now. Here is roughly the same bytecode as above, but simpler, side-by-side with the unparsed code:

```r
function() {      |
                  | BBO:
  x <- 10         |   ldconst 1 -> %1.2; goto BB1
                  | BB1:
  y <- 1          |   ldconst 10 -> %1.3
  while (y < x) { |   lt %1.2 %1.3 -> %1.5; branch %1.5 BB3 BB2
                  | BB3:
    y <- y * 2    |   ldconst 2 -> %3.0; mul %1.2 %3.0 -> %1.2
  }               |   goto BB1
                  | BB2:
  y               |   return %1.2
}                 |
```

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

## What about PIR?

PIR is another representation of R. However, PIR is *not* a bytecode, and PIR code isn't interpreted - instead, it's designed to be optimized. While interpreting, the compiler converts RIR code into PIR, then performs some optimization passes, then converts the PIR back into RIR - *assuming the compiler works*, the new RIR code is semantically the same as the old, but faster. The RIR interpreter also performs some simple optimizations directly when compiling R into RIR.

For more information about PIR, see [pir.md](./pir.md)
