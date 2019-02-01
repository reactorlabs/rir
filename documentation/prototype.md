# Prototyping JIT Optimizations in R

RIR adds some extra capabilities to R, designed to allow you to prototype JIT optmizations. Using these capabilities, you can implement an optimization's logic directly in R, instead of C++.

## Why Prototype

R is slow, and the extra capabilities make it slower, so this "optimization" won't actually speed up anything and can't be used in practice. However, prototyping is useful because:

- It might be faster and easier to debug the logic in R instead of C++. For example, you don't need to worry about segfaults, and you don't need to recompile.
- You can prove that an optimization is *semantically* equivalent, without having to actually implement it. Of course, you would also need to show that the optimization would be faster, but for some optimizations this shouldn't be too hard;

Prototyping is more useful for more complex optimizations.

## How to Use

RIR contains 2 special functions - `rir.enablePrototype()` and `rir.disablePrototype()` - which enable/disable the special capabilities. When `rir.enablePrototype()` is on:

- Before a variable is assigned, `rir.onModify` will be called with the symbol, environment, and new value.
- (TODO) `rir.callLocId` returns an integer identifier for the enclosing function, which is unique for every bytecode instruction calling the function. For example, in `foo <- function() { rir.callLocId }; for (i in 1:5) { foo() }; foo(); foo()`, the first 5 invocations of `foo` might return one identifier, the 6th invocation will return a different identifier, and the 7th invocation will return a third identifier.
