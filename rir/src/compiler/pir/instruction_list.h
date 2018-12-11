#ifndef COMPILER_INSTRUCTION_LIST_H
#define COMPILER_INSTRUCTION_LIST_H

#include "../../simple_instruction_list.h"

// Please keep in sync with implementation of instructions in instruction.h

#define V_SIMPLE_INSTRUCTION_IN_COMPILER_INSTRUCTIONS(V, name, Name) V(Name)

#define BINOP_INSTRUCTIONS(V)                                                  \
    V(Lte)                                                                     \
    V(Gte)                                                                     \
    V(Lt)                                                                      \
    V(Gt)                                                                      \
    V(Mod)                                                                     \
    V(Add)                                                                     \
    V(Div)                                                                     \
    V(IDiv)                                                                    \
    V(Colon)                                                                   \
    V(Pow)                                                                     \
    V(Sub)                                                                     \
    V(Mul)                                                                     \
    V(Neq)                                                                     \
    V(Eq)                                                                      \
    V(Extract1_1D)                                                             \
    V(Extract2_1D)

#define COMPILER_INSTRUCTIONS(V)                                               \
    SIMPLE_INSTRUCTIONS(V_SIMPLE_INSTRUCTION_IN_COMPILER_INSTRUCTIONS, V)      \
    BINOP_INSTRUCTIONS(V)                                                      \
    V(LdFun)                                                                   \
    V(Seq)                                                                     \
    V(LdVar)                                                                   \
    V(LdConst)                                                                 \
    V(LdArg)                                                                   \
    V(StVarSuper)                                                              \
    V(LdVarSuper)                                                              \
    V(StVar)                                                                   \
    V(Branch)                                                                  \
    V(Phi)                                                                     \
    V(AsLogical)                                                               \
    V(AsTest)                                                                  \
    V(IsObject)                                                                \
    V(Return)                                                                  \
    V(MkArg)                                                                   \
    V(MkFunCls)                                                                \
    V(MkCls)                                                                   \
    V(ChkMissing)                                                              \
    V(ChkClosure)                                                              \
    V(Call)                                                                    \
    V(Extract2_2D)                                                             \
    V(Extract1_2D)                                                             \
    V(NamedCall)                                                               \
    V(StaticCall)                                                              \
    V(CallBuiltin)                                                             \
    V(CallSafeBuiltin)                                                         \
    V(CallImplicit)                                                            \
    V(MkEnv)                                                                   \
    V(LdFunctionEnv)                                                           \
    V(LAnd)                                                                    \
    V(LOr)                                                                     \
    V(Inc)                                                                     \
    V(Not)                                                                     \
    V(Is)                                                                      \
    V(Plus)                                                                    \
    V(Minus)                                                                   \
    V(Identical)                                                               \
    V(Length)                                                                  \
    V(Subassign1_1D)                                                           \
    V(Subassign2_1D)                                                           \
    V(ForSeqSize)                                                              \
    V(FrameState)                                                              \
    V(Checkpoint)                                                              \
    V(Assume)                                                                  \
    V(Deopt)                                                                   \
    V(ScheduledDeopt)                                                          \
    V(Force)                                                                   \
    V(CastType)                                                                \
    V(SetShared)                                                               \
    V(EnsureNamed)                                                             \
    V(PirCopy)

#endif
