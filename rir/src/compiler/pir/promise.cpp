#include "promise.h"
#include "interpreter/runtime.h"

namespace rir {
namespace pir {

Promise::Promise(ClosureVersion* owner, unsigned id, unsigned src)
    : id(id), owner(owner), srcPoolIdx_(src) {
    assert(src_pool_at(globalContext(), srcPoolIdx_));
}

unsigned Promise::srcPoolIdx() const { return srcPoolIdx_; }
} // namespace pir
} // namespace rir
