#include "promise.h"
#include "interpreter/instance.h"
#include "ir/BC.h"

namespace rir {
namespace pir {

Promise::Promise(ClosureVersion* owner, unsigned id, rir::Code* rirSrc)
    : id(id), owner(owner), rirSrc_(rirSrc), srcPoolIdx_(rirSrc->src) {
    assert(src_pool_at(globalContext(), srcPoolIdx_));
}

unsigned Promise::srcPoolIdx() const { return srcPoolIdx_; }
} // namespace pir
} // namespace rir
