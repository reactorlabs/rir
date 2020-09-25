#include "pass.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/promise.h"

namespace rir {
namespace pir {

bool Pass::apply(Compiler& cmp, ClosureVersion* function,
                 LogStream& log) const {
    bool res = apply(cmp, function, function, log);
    if (runOnPromises()) {
        function->eachPromise(
            [&](Promise* p) { res = apply(cmp, function, p, log) && res; });
    }
    changedAnything_ = res;
    return res;
}

} // namespace pir
} // namespace rir
