#ifndef RIR_PRIMITIVES_H
#define RIR_PRIMITIVES_H

#include "RDefs.h"
#include "Runtime.h"

namespace rjit {
namespace rir {

class Primitives {
  public:
    static BCClosure* compilePrimitive(SEXP fun, num_args_t nargs);

    static constexpr long do_if_id = 0;
    static constexpr long do_begin_id = 11;
    static constexpr long do_set_id = 8;
    static constexpr long do_substitute_id = 30;
};

} // rir
} // rjit

#endif
