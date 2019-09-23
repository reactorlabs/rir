#ifndef BC_NOARG_LIST_H
#define BC_NOARG_LIST_H

#include "simple_instruction_list.h"

#define V_SIMPLE_INSTRUCTION_IN_BC_NOARGS(V, name, Name) V(_, name, name)

#define BC_NOARGS(V, NESTED)                                                   \
    SIMPLE_INSTRUCTIONS(V_SIMPLE_INSTRUCTION_IN_BC_NOARGS, V)                  \
    V(NESTED, updatePromise, update_promise)                                   \
    V(NESTED, nop, nop)                                                        \
    V(NESTED, parentEnv, parent_env)                                           \
    V(NESTED, getEnv, get_env)                                                 \
    V(NESTED, setEnv, set_env)                                                 \
    V(NESTED, ret, ret)                                                        \
    V(NESTED, pop, pop)                                                        \
    V(NESTED, force, force)                                                    \
    V(NESTED, asast, asast)                                                    \
    V(NESTED, checkMissing, check_missing)                                     \
    V(NESTED, subassign1_1, subassign1_1)                                      \
    V(NESTED, subassign2_1, subassign2_1)                                      \
    V(NESTED, subassign1_2, subassign1_2)                                      \
    V(NESTED, subassign2_2, subassign2_2)                                      \
    V(NESTED, length, length)                                                  \
    V(NESTED, names, names)                                                    \
    V(NESTED, setNames, set_names)                                             \
    V(NESTED, asbool, asbool)                                                  \
    V(NESTED, endloop, endloop)                                                \
    V(NESTED, dup, dup)                                                        \
    V(NESTED, dup2, dup2)                                                      \
    V(NESTED, forSeqSize, for_seq_size)                                        \
    V(NESTED, inc, inc)                                                        \
    V(NESTED, dec, dec)                                                        \
    V(NESTED, close, close)                                                    \
    V(NESTED, add, add)                                                        \
    V(NESTED, mul, mul)                                                        \
    V(NESTED, div, div)                                                        \
    V(NESTED, pow, pow)                                                        \
    V(NESTED, idiv, idiv)                                                      \
    V(NESTED, mod, mod)                                                        \
    V(NESTED, sub, sub)                                                        \
    V(NESTED, uplus, uplus)                                                    \
    V(NESTED, uminus, uminus)                                                  \
    V(NESTED, not_, not)                                                       \
    V(NESTED, lt, lt)                                                          \
    V(NESTED, gt, gt)                                                          \
    V(NESTED, le, le)                                                          \
    V(NESTED, ge, ge)                                                          \
    V(NESTED, eq, eq)                                                          \
    V(NESTED, identicalNoforce, identical_noforce)                             \
    V(NESTED, ne, ne)                                                          \
    V(NESTED, colon, colon)                                                    \
    V(NESTED, setShared, set_shared)                                           \
    V(NESTED, ensureNamed, ensure_named)                                       \
    V(NESTED, asLogical, aslogical)                                            \
    V(NESTED, ceil, ceil)                                                      \
    V(NESTED, floor, floor)                                                    \
    V(NESTED, lglOr, lgl_or)                                                   \
    V(NESTED, lglAnd, lgl_and)                                                 \
    V(NESTED, isfun, isfun)                                                    \
    V(NESTED, invisible, invisible)                                            \
    V(NESTED, visible, visible)                                                \
    V(NESTED, extract1_1, extract1_1)                                          \
    V(NESTED, extract1_2, extract1_2)                                          \
    V(NESTED, extract2_1, extract2_1)                                          \
    V(NESTED, extract2_2, extract2_2)                                          \
    V(NESTED, swap, swap)                                                      \
    V(NESTED, isstubenv, isstubenv)                                            \
    V(NESTED, return_, return )

#undef V_SIMPLE_INSTRUCTION

#endif
