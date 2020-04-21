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
    V(Eq)

#define VECTOR_RW_INSTRUCTIONS(V)                                              \
    V(Extract1_1D)                                                             \
    V(Extract2_1D)                                                             \
    V(Extract2_2D)                                                             \
    V(Extract1_2D)                                                             \
    V(Extract1_3D)                                                             \
    V(Subassign1_1D)                                                           \
    V(Subassign2_1D)                                                           \
    V(Subassign1_2D)                                                           \
    V(Subassign2_2D)                                                           \
    V(Subassign1_3D)

#define COMPILER_INSTRUCTIONS(V)                                               \
    SIMPLE_INSTRUCTIONS(V_SIMPLE_INSTRUCTION_IN_COMPILER_INSTRUCTIONS, V)      \
    BINOP_INSTRUCTIONS(V)                                                      \
    VECTOR_RW_INSTRUCTIONS(V)                                                  \
    V(LdFun)                                                                   \
    V(LdVar)                                                                   \
    V(LdConst)                                                                 \
    V(LdArg)                                                                   \
    V(LdDots)                                                                  \
    V(StVarSuper)                                                              \
    V(LdVarSuper)                                                              \
    V(StVar)                                                                   \
    V(Branch)                                                                  \
    V(Phi)                                                                     \
    V(DotsList)                                                                \
    V(ExpandDots)                                                              \
    V(AsLogical)                                                               \
    V(AsTest)                                                                  \
    V(ColonInputEffects)                                                       \
    V(ColonCastLhs)                                                            \
    V(ColonCastRhs)                                                            \
    V(IsEnvStub)                                                               \
    V(Return)                                                                  \
    V(NonLocalReturn)                                                          \
    V(MkArg)                                                                   \
    V(UpdatePromise)                                                           \
    V(MkFunCls)                                                                \
    V(MkCls)                                                                   \
    V(ChkMissing)                                                              \
    V(ChkClosure)                                                              \
    V(Call)                                                                    \
    V(NamedCall)                                                               \
    V(StaticCall)                                                              \
    V(CallBuiltin)                                                             \
    V(CallSafeBuiltin)                                                         \
    V(MkEnv)                                                                   \
    V(MaterializeEnv)                                                          \
    V(PushContext)                                                             \
    V(PopContext)                                                              \
    V(LdFunctionEnv)                                                           \
    V(LAnd)                                                                    \
    V(LOr)                                                                     \
    V(Not)                                                                     \
    V(Inc)                                                                     \
    V(Is)                                                                      \
    V(IsType)                                                                  \
    V(Plus)                                                                    \
    V(Minus)                                                                   \
    V(Identical)                                                               \
    V(ForSeqSize)                                                              \
    V(XLength)                                                                 \
    V(FrameState)                                                              \
    V(Checkpoint)                                                              \
    V(Assume)                                                                  \
    V(Deopt)                                                                   \
    V(ScheduledDeopt)                                                          \
    V(Force)                                                                   \
    V(CastType)                                                                \
    V(Missing)                                                                 \
    V(Visible)                                                                 \
    V(Invisible)                                                               \
    V(Names)                                                                   \
    V(SetNames)                                                                \
    V(PirCopy)                                                                 \
    V(RecordDeoptReason)                                                       \
    V(Nop)

#endif
