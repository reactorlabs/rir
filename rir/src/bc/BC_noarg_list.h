#ifndef BC_NOARG_LIST_H
#define BC_NOARG_LIST_H

#include "simple_instruction_list.h"

#define V_SIMPLE_INSTRUCTION_IN_BC_NOARGS(V, name, Name) V(_, name, name)

#define BC_NOARGS(V, NESTED)                                                   \
    SIMPLE_INSTRUCTIONS(V_SIMPLE_INSTRUCTION_IN_BC_NOARGS, V)                  \
    V(NESTED, as_switch_idx, as_switch_idx)                                    \
    V(NESTED, nop, nop)                                                        \
    V(NESTED, ret, ret)                                                        \
    V(NESTED, pop, pop)                                                        \
    V(NESTED, force, force)                                                    \
    V(NESTED, set_vec_elt, set_vec_elt)                                        \
    V(NESTED, subassign1_1, subassign1_1)                                      \
    V(NESTED, subassign2_1, subassign2_1)                                      \
    V(NESTED, subassign1_2, subassign1_2)                                      \
    V(NESTED, subassign2_2, subassign2_2)                                      \
    V(NESTED, subassign1_3, subassign1_3)                                      \
    V(NESTED, names, names)                                                    \
    V(NESTED, setNames, set_names)                                             \
    V(NESTED, asbool, asbool)                                                  \
    V(NESTED, endloop, endloop)                                                \
    V(NESTED, dup, dup)                                                        \
    V(NESTED, dup2, dup2)                                                      \
    V(NESTED, forSeqSize, for_seq_size)                                        \
    V(NESTED, length_, length)                                                 \
    V(NESTED, close, close)                                                    \
    V(NESTED, inc, inc)                                                        \
    V(NESTED, uplus, uplus)                                                    \
    V(NESTED, uminus, uminus)                                                  \
    V(NESTED, not_, not )                                                      \
    V(NESTED, add, add)                                                        \
    V(NESTED, sub, sub)                                                        \
    V(NESTED, mul, mul)                                                        \
    V(NESTED, div, div)                                                        \
    V(NESTED, idiv, idiv)                                                      \
    V(NESTED, mod, mod)                                                        \
    V(NESTED, pow, pow)                                                        \
    V(NESTED, eq, eq)                                                          \
    V(NESTED, ne, ne)                                                          \
    V(NESTED, lt, lt)                                                          \
    V(NESTED, le, le)                                                          \
    V(NESTED, gt, gt)                                                          \
    V(NESTED, ge, ge)                                                          \
    V(NESTED, lglAnd, lgl_and)                                                 \
    V(NESTED, lglOr, lgl_or)                                                   \
    V(NESTED, colon, colon)                                                    \
    V(NESTED, identicalNoforce, identical_noforce)                             \
    V(NESTED, setShared, set_shared)                                           \
    V(NESTED, ensureNamed, ensure_named)                                       \
    V(NESTED, aslogical, aslogical)                                            \
    V(NESTED, checkFunction, check_function)                                   \
    V(NESTED, invisible, invisible)                                            \
    V(NESTED, visible, visible)                                                \
    V(NESTED, extract1_1, extract1_1)                                          \
    V(NESTED, extract1_2, extract1_2)                                          \
    V(NESTED, extract1_3, extract1_3)                                          \
    V(NESTED, extract2_1, extract2_1)                                          \
    V(NESTED, extract2_2, extract2_2)                                          \
    V(NESTED, swap, swap)                                                      \
    V(NESTED, return_, return )                                                \
    V(NESTED, colonInputEffects, colon_input_effects)                          \
    V(NESTED, colonCastLhs, colon_cast_lhs)                                    \
    V(NESTED, colonCastRhs, colon_cast_rhs)

#undef V_SIMPLE_INSTRUCTION

#endif
