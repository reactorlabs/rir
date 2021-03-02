#ifndef COMPILER_VALUE_LIST_H
#define COMPILER_VALUE_LIST_H

#define COMPILER_VALUES(V)                                                     \
    V(True)                                                                    \
    V(False)                                                                   \
    V(OpaqueTrue)                                                              \
    V(NaLogical)                                                               \
    V(Tombstone)                                                               \
    V(MissingArg)                                                              \
    V(UnboundValue)                                                            \
    V(Env)                                                                     \
    V(Nil)

#endif
