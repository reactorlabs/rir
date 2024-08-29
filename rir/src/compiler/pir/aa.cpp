#include "aa.h"

#include "pir_impl.h"

namespace rir {
namespace pir {

void AA::copyInfo(const PirType* fromType, const PirType* toType) {

    if (!currentVersion)
        return;

    auto& ldArgsFrom = currentVersion->relatedInstructions[fromType];
    auto& ldArgsTo = currentVersion->relatedInstructions[toType];
    ldArgsTo.insert(ldArgsFrom.begin(), ldArgsFrom.end());
}

void AA::unregisterType(PirType* type) {
    if (!currentVersion)
        return;

    currentVersion->relatedInstructions.erase(type);
}

void AA::recordNotObject(const PirType* type) {

    if (!currentVersion)
        return;

    auto& ldArgs = currentVersion->relatedInstructions[type];
    for (auto ldArg : ldArgs) {
        ldArg->notObj = true;
    }
}
void AA::recordEager(const PirType* type) {

    if (!currentVersion)
        return;

    auto& ldArgs = currentVersion->relatedInstructions[type];
    for (auto ldArg : ldArgs) {
        ldArg->eager = true;
    }
}

void AA::recordSimpleScalar(const PirType* type) {

    if (!currentVersion)
        return;

    auto& ldArgs = currentVersion->relatedInstructions[type];
    for (auto ldArg : ldArgs) {
        ldArg->simpleScalar = true;
    }
}

void AA::setCurrentVersion(ClosureVersion* v) { currentVersion = v; }

AA* AA::singleInstance{nullptr};

} // namespace pir
} // namespace rir