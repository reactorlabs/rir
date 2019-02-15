# RIR

## Overview

RIR is a bytecode, similar to assembly or the JVM bytecode.

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

TODO
