#ifndef SYMBOLS_H_
#define SYMBOLS_H_

#include "r.h"
#include "symbol_list.h"

namespace rir {

namespace symbol {
#define V(name, txt) extern SEXP name;
SYMBOLS(V)
#undef V
} // namespace symbol

} // namespace rir

#endif // SYMBOLS_H_
