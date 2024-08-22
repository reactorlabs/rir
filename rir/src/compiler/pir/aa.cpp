#include "aa.h"

#include "pir_impl.h"

namespace rir {
namespace pir {

void AA::copyInfo(const PirType* fromType, const PirType* toType) {
    auto& ldArgsFrom = currentVersion->relatedInstructions[fromType];
    auto& ldArgsTo = currentVersion->relatedInstructions[toType];
    ldArgsTo.insert(ldArgsFrom.begin(), ldArgsFrom.end());
}

void AA::recordNotObject(const PirType* type) {

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