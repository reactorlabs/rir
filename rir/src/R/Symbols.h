#ifndef SYMBOLS_H_
#define SYMBOLS_H_

#include "r.h"
#include "symbols_list.h"

namespace rir {

namespace symbol {
#define V(name, txt) extern SEXP name;
SYMBOLS(V)
#undef V
} // namespace symbol

} // namespace rjit

#endif // SYMBOLS_H_
