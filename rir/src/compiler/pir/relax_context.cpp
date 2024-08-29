#include "relax_context.h"

#include "pir_impl.h"

namespace rir {
namespace pir {

void RelaxContext::copyInfo(const PirType* fromType, const PirType* toType) {

    if (!currentVersion)
        return;

    auto& ldArgsFrom = currentVersion->relatedInstructions[fromType];
    auto& ldArgsTo = currentVersion->relatedInstructions[toType];
    ldArgsTo.insert(ldArgsFrom.begin(), ldArgsFrom.end());
}

void RelaxContext::unregisterType(PirType* type) {
    if (!currentVersion)
        return;

    currentVersion->relatedInstructions.erase(type);
}

void RelaxContext::recordNotObject(const PirType* type) {

    if (!currentVersion)
        return;

    auto& ldArgs = currentVersion->relatedInstructions[type];
    for (auto ldArg : ldArgs) {
        ldArg->notObj = true;
    }
}
void RelaxContext::recordEager(const PirType* type) {

    if (!currentVersion)
        return;

    auto& ldArgs = currentVersion->relatedInstructions[type];
    for (auto ldArg : ldArgs) {
        ldArg->eager = true;
    }
}

void RelaxContext::recordSimpleScalar(const PirType* type) {

    if (!currentVersion)
        return;

    auto& ldArgs = currentVersion->relatedInstructions[type];
    for (auto ldArg : ldArgs) {
        ldArg->simpleScalar = true;
    }
}

void RelaxContext::setCurrentVersion(ClosureVersion* v) { currentVersion = v; }

RelaxContext* RelaxContext::singleInstance{nullptr};

} // namespace pir
} // namespace rir