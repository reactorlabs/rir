# RIR

## Overview

RIR is a bytecode, similar to assembly or the JVM bytecode. It stands for "R Intermediate Representation".

R is an interpreted language, so every line of code is executed immediately when it's parsed. However, re-parsing every single statement is slow. So, as the RIR interpreter parses code, it first converts it into "RIR bytecode", then executes it. When a call to previously-parsed code is encountered, like in a loop or function call, instead of re-parsing the code, RIR will simply run the existing bytecode. Here is a visualization of how the following might be interpreted (the `>`s denote what the interpreter is reading, note that the actual bytecode generated is more complex):

```r
cat("[")
for (i in 1:3) {
  cat(as.character(i))
}
cat("]")
```

```r
> cat("[")              | > ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
for (i in 1:3) {
  cat(as.character(i))
}
cat("]")
```

```r
cat("[")                | ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
> for (i in 1:3) {      | > [BB2] push 3; push 1; colon; forSeqSize; ...
  cat(as.character(i))
}
cat("]")
```

```r
cat("[")                | ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
for (i in 1:3) {        | [BB2] push 3; push 1; colon; forSeqSize; ...
>  cat(as.character(i)) | > ldfun "as.character" -> %0.0; ldvar "i" -> %0.1; call %0.0(%0.1) -> %0.2; ldfun "cat" -> %0.0; call %0.0(%0.2) -> %0.3
}
cat("]")
```

```r
cat("[")                | ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
for (i in 1:3) {        | [BB2] push 3; push 1; colon; forSeqSize; ...
  cat(as.character(i))  | ldfun "as.character" -> %0.0; ldvar "i" -> %0.1; call %0.0(%0.1) -> %0.2; ldfun "cat" -> %0.0; call %0.0(%0.2) -> %0.3
> }                     | > ...; branch BB3 BB2
cat("]")
```

Notice that the interpreter stopped parsing code, now it's just running bytecode.

```r
cat("[")                | ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
for (i in 1:3) {        | > [BB2] push 3; push 1; colon; forSeqSize; ...
  cat(as.character(i))  | ldfun "as.character" -> %0.0; ldvar "i" -> %0.1; call %0.0(%0.1) -> %0.2; ldfun "cat" -> %0.0; call %0.0(%0.2) -> %0.3
}                       | ...; branch BB3 BB2
cat("]")
```

```r
cat("[")                | ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
for (i in 1:3) {        | [BB2] push 3; push 1; colon; forSeqSize; ...
  cat(as.character(i))  | > ldfun "as.character" -> %0.0; ldvar "i" -> %0.1; call %0.0(%0.1) -> %0.2; ldfun "cat" -> %0.0; call %0.0(%0.2) -> %0.3
}                       | ...; branch BB3 BB2
cat("]")
```

```r
cat("[")                | ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
for (i in 1:3) {        | [BB2] push 3; push 1; colon; forSeqSize; ...
  cat(as.character(i))  | ldfun "as.character" -> %0.0; ldvar "i" -> %0.1; call %0.0(%0.1) -> %0.2; ldfun "cat" -> %0.0; call %0.0(%0.2) -> %0.3
}                       | > ...; branch BB3 BB2
cat("]")
```

```r
cat("[")                | ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
for (i in 1:3) {        | > [BB2] push 3; push 1; colon; forSeqSize; ...
  cat(as.character(i))  | ldfun "as.character" -> %0.0; ldvar "i" -> %0.1; call %0.0(%0.1) -> %0.2; ldfun "cat" -> %0.0; call %0.0(%0.2) -> %0.3
}                       | ...; branch BB3 BB2
cat("]")
```

```r
cat("[")                | ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
for (i in 1:3) {        | [BB2] push 3; push 1; colon; forSeqSize; ...
  cat(as.character(i))  | > ldfun "as.character" -> %0.0; ldvar "i" -> %0.1; call %0.0(%0.1) -> %0.2; ldfun "cat" -> %0.0; call %0.0(%0.2) -> %0.3
}                       | ...; branch BB3 BB2
cat("]")
```

```r
cat("[")                | ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
for (i in 1:3) {        | [BB2] push 3; push 1; colon; forSeqSize; ...
  cat(as.character(i))  | ldfun "as.character" -> %0.0; ldvar "i" -> %0.1; call %0.0(%0.1) -> %0.2; ldfun "cat" -> %0.0; call %0.0(%0.2) -> %0.3
}                       | > ...; branch BB3 BB2
cat("]")
```

```r
cat("[")                | ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
for (i in 1:3) {        | [BB2] push 3; push 1; colon; forSeqSize; ...
  cat(as.character(i))  | ldfun "as.character" -> %0.0; ldvar "i" -> %0.1; call %0.0(%0.1) -> %0.2; ldfun "cat" -> %0.0; call %0.0(%0.2) -> %0.3
}                       | ...; branch BB3 BB2
> cat("]")              | > ldfun "cat" -> %0.0; push "[" -> %0.1; call %0.0(%0.1) -> %0.2
```

## Code Structure

Here are a few important files and functions:

- `Compiler.cpp`: Converts the parsed R AST into RIR (we don't parse the AST ourselves)
  - `compileSpecialCall`: Converts calls to certain, "special" functions (e.g. `+`, `for`) into their own bytecodes. This way we can interpret them faster.
- `interp.cpp` - `evalRirCode`: Contains the entire RIR interpreter

## Comparison to GNU-R

The default R interpreter (GNU-R) also has a bytecode. The main difference between this bytecode and RIR is that GNU-R has a few "fat" instructions, which are more complicated, while RIR has many more instructions, but they're simpler. For example, RIR has explicit instructions for creating environments, but GNU-R doesn't.

## What about PIR?

PIR is another representation of R, similar to RIR. However, PIR code isn't interpreted - instead, it's designed to be optimized. While interpreting, compiler converts RIR code into PIR, then performs some optimization passes, then converts the PIR back into RIR - *assuming the compiler works*, the new RIR code is semantically the same as the old, but faster.

Not all RIR code is converted into PIR, only code which is re-used a lot (currently, when a function is called 3 times it gets optimized). This is because, the PIR optimizations themselves take a long time (remember, this is during interpretation) - if an instruction is only run once, it's faster to just run it unoptimized. Furthermore, PIR performs **speculative optimizations**, and it could use the results of previous iterations to formi its assumptions.

The RIR interpreter also performs some simple optimizations directly when converting from R into RIR, and while interpreting RIR bytecode.

For more information about PIR, see [pir.md](../pir.md)

### Speculative Optimization

The R programming language is very hard to optimize because you can't make even seemingly-obvious assumptions, there are many "edge-cases". For example, in `a + b`, `a` could be a promise that, when read, modifies `b`. Or, in `x <- 5; f(); print(x)`, `f` could change the value of `x`, or even delete it so that `print` gives a "missing value" error, no matter where `f` was originally defined.

As a result, PIR's optimizations are speculative. First, PIR makes assumptions - e.g. that `a` doesn't perform any side effects, or `f` doesn't modify variables outside its environment. Then, PIR performs optimizations which would only work under these assumptions, *except* there are checks before the optimizations. The checks are evaluated at runtime, and if they fail, PIR will "deoptimize", and instead of running the (invalid) optimized code, it'll run the original RIR bytecode, or another version with less assumptions.
