#ifndef SIMPLE_INSTRUCTION_LIST_H
#define SIMPLE_INSTRUCTION_LIST_H

// A "simple instruction" is a bytecode instruction which is created with
// "rir.<instruction name>()". It doesn't take any arguments or do anything
// unique ("fancy" or different than other simple instructions), except that
// it's interpreted differently. For example, `int3` creates a breakpoint, and
// `printInvocation` prints the calling function's invocation count. Instead
// of modifying a lot of files, to add a simple instruction:
//
// - Add a statement here - V(NESTED, <name lower case>, <name upper case>):
//   this declares the instruction
// - Add an INSTRUCTION(<name lower case>_) { ... } to interp.cpp:
//   this defines how to interpret the instruction

#define SIMPLE_INSTRUCTIONS(V, NESTED)                                         \
V(NESTED, int3, Int3)                                                          \
V(NESTED, printInvocation, PrintInvocation)                                    \

#endif
