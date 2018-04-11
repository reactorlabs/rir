#ifndef COMPILER_INSTRUCTION_LIST_H
#define COMPILER_INSTRUCTION_LIST_H

// Please keep in sync with implementation of instructions in instruction.h

#define COMPILER_INSTRUCTIONS(V)                                               \
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
    V(CallBuiltin)                                                             \
    V(CallSafeBuiltin)                                                         \
    V(MkEnv)                                                                   \
    V(LdFunctionEnv)                                                           \
    V(Lte)                                                                     \
    V(Gte)                                                                     \
    V(LAnd)                                                                    \
    V(LOr)                                                                     \
    V(Mod)                                                                     \
    V(Add)                                                                     \
    V(Div)                                                                     \
    V(IDiv)                                                                    \
    V(Colon)                                                                   \
    V(Pow)                                                                     \
    V(Sub)                                                                     \
    V(Mul)                                                                     \
    V(Inc)                                                                     \
    V(Not)                                                                     \
    V(Is)                                                                      \
    V(Plus)                                                                    \
    V(Minus)                                                                   \
    V(Lt)                                                                      \
    V(Gt)                                                                      \
    V(Neq)                                                                     \
    V(Eq)                                                                      \
    V(Length)                                                                  \
    V(Extract1_1D)                                                             \
    V(Extract1_2D)                                                             \
    V(Extract2_1D)                                                             \
    V(Extract2_2D)                                                             \
    V(Subassign1_1D)                                                           \
    V(Subassign2_1D)                                                           \
    V(ForSeqSize)                                                              \
    V(Deopt)                                                                   \
    V(Force)                                                                   

#endif
