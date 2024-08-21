#include "aa.h"

#include "pir_impl.h"

namespace rir {
namespace pir {

void AA::notMaybeObject(PirType* type) {

    if (!currentVersion)
        return;

    auto& ldArgs = currentVersion->relatedInstructions[type];
    for (auto ldArg : ldArgs) {
        ldArg->notObj = true;
    }
}

void AA::setCurrentVersion(ClosureVersion* v) { currentVersion = v; }

AA* AA::singleInstance{nullptr};

} // namespace pir
} // namespace rir