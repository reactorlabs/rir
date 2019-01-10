#include "Symbols.h"

namespace rir {

namespace symbol {
#define V(name, txt) SEXP name = Rf_install(txt);
SYMBOLS(V)
#undef V
} // namespace symbol

} // namespace rir
