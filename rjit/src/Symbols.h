#ifndef SYMBOLS_H_
#define SYMBOLS_H_

#include "RDefs.h"

namespace rjit {

namespace symbol {
#define DECLARE(name, txt) extern SEXP name

DECLARE(SuperAssignBracket, "[<<-");
DECLARE(SuperAssignDoubleBracket, "[[<<-");
DECLARE(AssignBracket, "[<-");
DECLARE(AssignDoubleBracket, "[[<-");
DECLARE(DoubleBracket, "[[");
DECLARE(Bracket, "[");
DECLARE(Block, "{");
DECLARE(Parenthesis, "(");
DECLARE(Assign, "<-");
DECLARE(Assign2, "=");
DECLARE(SuperAssign, "<<-");
DECLARE(If, "if");
DECLARE(Function, "function");
DECLARE(Return, "return");
DECLARE(For, "for");
DECLARE(While, "while");
DECLARE(Repeat, "repeat");
DECLARE(Break, "break");
DECLARE(Next, "next");
DECLARE(Switch, "switch");
DECLARE(Add, "+");
DECLARE(Sub, "-");
DECLARE(Mul, "*");
DECLARE(Div, "/");
DECLARE(Pow, "^");
DECLARE(Sqrt, "sqrt");
DECLARE(Exp, "exp");
DECLARE(Eq, "==");
DECLARE(Ne, "!=");
DECLARE(Lt, "<");
DECLARE(Le, "<=");
DECLARE(Ge, ">=");
DECLARE(Gt, ">");
DECLARE(BitAnd, "&");
DECLARE(BitOr, "|");
DECLARE(Not, "!");
DECLARE(Ellipsis, "...");

#undef DECLARE
} // namespace symbol

} // namespace rjit

#endif // SYMBOLS_H_
