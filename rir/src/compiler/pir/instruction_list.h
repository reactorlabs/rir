#ifndef COMPILER_INSTRUCTION_LIST_H
#define COMPILER_INSTRUCTION_LIST_H

#include "../../simple_instruction_list.h"

// Please keep in sync with implementation of instructions in instruction.h

#define V_SIMPLE_INSTRUCTION_IN_COMPILER_INSTRUCTIONS(V, name, Name) V(Name)

#define ARITHMETIC_UNOP_INSTRUCTIONS(V)                                        \
    V(Plus)                                                                    \
    V(Minus)

#define LOGICAL_UNOP_INSTRUCTIONS(V) V(Not)

#define UNOP_INSTRUCTIONS(V)                                                   \
    ARITHMETIC_UNOP_INSTRUCTIONS(V)                                            \
    LOGICAL_UNOP_INSTRUCTIONS(V)

#define ARITHMETIC_BINOP_INSTRUCTIONS(V)                                       \
    V(Add)                                                                     \
    V(Sub)                                                                     \
    V(Mul)                                                                     \
    V(Div)                                                                     \
    V(IDiv)                                                                    \
    V(Mod)                                                                     \
    V(Pow)

#define RELATIONAL_BINOP_INSTRUCTIONS(V)                                       \
    V(Eq)                                                                      \
    V(Neq)                                                                     \
    V(Lt)                                                                      \
    V(Lte)                                                                     \
    V(Gt)                                                                      \
    V(Gte)

#define LOGICAL_BINOP_INSTRUCTIONS(V)                                          \
    V(LAnd)                                                                    \
    V(LOr)

#define BINOP_INSTRUCTIONS(V)                                                  \
    ARITHMETIC_BINOP_INSTRUCTIONS(V)                                           \
    RELATIONAL_BINOP_INSTRUCTIONS(V)                                           \
    LOGICAL_BINOP_INSTRUCTIONS(V)                                              \
    V(Colon)

#define VECTOR_EXTRACT_INSTRUCTIONS(V)                                         \
    V(Extract1_1D)                                                             \
    V(Extract2_1D)                                                             \
    V(Extract1_2D)                                                             \
    V(Extract2_2D)                                                             \
    V(Extract1_3D)

#define VECTOR_SUBASSIGN_INSTRUCTIONS(V)                                       \
    V(Subassign1_1D)                                                           \
    V(Subassign2_1D)                                                           \
    V(Subassign1_2D)                                                           \
    V(Subassign2_2D)                                                           \
    V(Subassign1_3D)

#define VECTOR_RW_INSTRUCTIONS(V)                                              \
    VECTOR_EXTRACT_INSTRUCTIONS(V)                                             \
    VECTOR_SUBASSIGN_INSTRUCTIONS(V)                                           \
    V(SetVecElt)

#define COMPILER_INSTRUCTIONS(V)                                               \
    SIMPLE_INSTRUCTIONS(V_SIMPLE_INSTRUCTION_IN_COMPILER_INSTRUCTIONS, V)      \
    UNOP_INSTRUCTIONS(V)                                                       \
    BINOP_INSTRUCTIONS(V)                                                      \
    VECTOR_RW_INSTRUCTIONS(V)                                                  \
    V(LdFun)                                                                   \
    V(LdVar)                                                                   \
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
    V(AsSwitchIdx)                                                             \
    V(CheckTrueFalse)                                                          \
    V(ColonInputEffects)                                                       \
    V(ColonCastLhs)                                                            \
    V(ColonCastRhs)                                                            \
    V(IsEnvStub)                                                               \
    V(Unreachable)                                                             \
    V(Return)                                                                  \
    V(NonLocalReturn)                                                          \
    V(MkArg)                                                                   \
    V(UpdatePromise)                                                           \
    V(MkCls)                                                                   \
    V(ChkMissing)                                                              \
    V(ChkFunction)                                                             \
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
    V(Inc)                                                                     \
    V(Is)                                                                      \
    V(IsType)                                                                  \
    V(Identical)                                                               \
    V(ToForSeq)                                                                \
    V(Length)                                                                  \
    V(FrameState)                                                              \
    V(Checkpoint)                                                              \
    V(Assume)                                                                  \
    V(Deopt)                                                                   \
    V(Force)                                                                   \
    V(CastType)                                                                \
    V(Missing)                                                                 \
    V(Visible)                                                                 \
    V(Invisible)                                                               \
    V(Names)                                                                   \
    V(SetNames)                                                                \
    V(PirCopy)                                                                 \
    V(Nop)

#endif
