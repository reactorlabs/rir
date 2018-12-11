#include "promise.h"
#include "interpreter/runtime.h"

namespace rir {
namespace pir {

Promise::Promise(Closure* fun, unsigned id, unsigned src)
    : id(id), fun(fun), srcPoolIdx_(src) {
    assert(src_pool_at(globalContext(), srcPoolIdx_));
}

unsigned Promise::srcPoolIdx() const { return srcPoolIdx_; }
} // namespace pir
} // namespace rir
