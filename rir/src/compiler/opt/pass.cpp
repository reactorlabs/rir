#include "pass.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/promise.h"

namespace rir {
namespace pir {

bool Pass::apply(Compiler& cmp, ClosureVersion* function, LogStream& log,
                 size_t iteration) const {
    bool res = apply(cmp, function, function, log, iteration);
    if (runOnPromises()) {
        function->eachPromise([&](Promise* p) {
            res = apply(cmp, function, p, log, iteration) && res;
        });
    }
    changedAnything_ = res;
    return res;
}

} // namespace pir
} // namespace rir
