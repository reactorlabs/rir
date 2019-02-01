#ifndef SIMPLE_INSTRUCTION_LIST_H
#define SIMPLE_INSTRUCTION_LIST_H

// Steps to define a simple instruction:
// - Add a statement here - V(NESTED, <name lower case>, <name upper case>):
//   this declares the instruction
// - Add an INSTRUCTION(<name lower case>_) { ... } to interp.cpp:
//   this defines how to interpret the instruction

#define SIMPLE_INSTRUCTIONS(V, NESTED)                                         \
    V(NESTED, int3, Int3)                                                      \
    V(NESTED, printInvocation, PrintInvocation)                                \
    V(NESTED, enablePrototype, EnablePrototype)                                \
    V(NESTED, disablePrototype, DisablePrototype)

#endif
