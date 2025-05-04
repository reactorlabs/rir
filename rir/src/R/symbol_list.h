#ifndef SYMBOLS_LIST_H_
#define SYMBOLS_LIST_H_

#include "simple_instruction_list.h"
#define SYMBOLS_SIMPLE_INSTRUCTION_V(V, name, _) V(name, "." #name)

#define SYMBOLS(V)                                                             \
    SIMPLE_INSTRUCTIONS(SYMBOLS_SIMPLE_INSTRUCTION_V, V)                       \
    V(UnknownDeoptTrigger, ".unknownDeoptTrigger")                             \
    V(SuperAssignBracket, "[<<-")                                              \
    V(SuperAssignDoubleBracket, "[[<<-")                                       \
    V(AssignBracket, "[<-")                                                    \
    V(AssignDoubleBracket, "[[<-")                                             \
    V(DoubleBracket, "[[")                                                     \
    V(Bracket, "[")                                                            \
    V(Block, "{")                                                              \
    V(Parenthesis, "(")                                                        \
    V(Assign, "<-")                                                            \
    V(Assign2, "=")                                                            \
    V(SuperAssign, "<<-")                                                      \
    V(If, "if")                                                                \
    V(Function, "function")                                                    \
    V(Return, "return")                                                        \
    V(For, "for")                                                              \
    V(While, "while")                                                          \
    V(Repeat, "repeat")                                                        \
    V(Break, "break")                                                          \
    V(Next, "next")                                                            \
    V(Switch, "switch")                                                        \
    V(Add, "+")                                                                \
    V(Sub, "-")                                                                \
    V(Not, "!")                                                                \
    V(Mul, "*")                                                                \
    V(Div, "/")                                                                \
    V(Idiv, "%/%")                                                             \
    V(Mod, "%%")                                                               \
    V(Pow, "^")                                                                \
    V(Eq, "==")                                                                \
    V(Ne, "!=")                                                                \
    V(Lt, "<")                                                                 \
    V(Le, "<=")                                                                \
    V(Ge, ">=")                                                                \
    V(Gt, ">")                                                                 \
    V(And, "&&")                                                               \
    V(Or, "||")                                                                \
    V(Colon, ":")                                                              \
    V(Sqrt, "sqrt")                                                            \
    V(Exp, "exp")                                                              \
    V(Ellipsis, "...")                                                         \
    V(Internal, ".Internal")                                                   \
    V(tmp, "*tmp*")                                                            \
    V(vtmp, "*vtmp*")                                                          \
    V(value, "value")                                                          \
    V(isnull, "is.null")                                                       \
    V(islist, "is.list")                                                       \
    V(ispairlist, "is.pairlist")                                               \
    V(quote, "quote")                                                          \
    V(Missing, "missing")                                                      \
    V(Invisible, "invisible")                                                  \
    V(seq, "seq")                                                              \
    V(lapply, "lapply")                                                        \
    V(aslist, "as.list")                                                       \
    V(ascharacter, "as.character")                                             \
    V(isvector, "is.vector")                                                   \
    V(substr, "substr")                                                        \
    V(Class, "class")                                                          \
    V(OldClass, "oldClass")                                                    \
    V(at, "@")                                                                 \
    V(names, "names")                                                          \
    V(attr, "attr")                                                            \
    V(body, "body")                                                            \
    V(slot, "slot")                                                            \
    V(as, "as")                                                                \
    V(packageSlot, "packageSlot")                                              \
    V(attributes, "attributes")                                                \
    V(c, "c")                                                                  \
    V(standardGeneric, "standardGeneric")                                      \
    V(dispatchGeneric, "dispatchGeneric")                                      \
    V(UseMethod, "UseMethod")                                                  \
    V(sysframe, "sys.frame")                                                   \
    V(syscall, "sys.call")                                                     \
    V(srcref, "srcref")                                                        \
    V(ambiguousCallTarget, ".ambiguousCallTarget.")                            \
    V(delayedEnv, ".delayedEnv.")                                              \
    V(eval, "eval")                                                            \
    V(lazyLoadDBexec, "lazyLoadDBexec")                                        \
    V(lazyLoadDBfetch, "lazyLoadDBfetch")                                      \
    V(all, "all")                                                              \
    V(FUN, "FUN")                                                              \
    V(forceAndCall, "forceAndCall")                                            \
    V(remove, "remove")                                                        \
    V(rm, "rm")                                                                \
    V(Recall, "Recall")                                                        \
    V(expandDotsTrigger, "\x02expandDotsTrigger\x03")
/*
 * The expandDotsTrigger symbol uses unprintable characters in hopes the users
 * won't create it from R (however, they still can, eg. `as.name("\x1a")`).
 * The other option is to make this a serializable EXTERNALSXP object.
 */

#endif // SYMBOLS_LIST_H_
