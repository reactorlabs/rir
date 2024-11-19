#include "relax_context.h"

#include "pir_impl.h"

namespace rir {
namespace pir {

void RelaxContext::copyInfo(const PirType* fromType, const PirType* toType) {
    if (!shouldRecord())
        return;

    auto& instructionsFrom = currentVersion()->relatedInstructions[fromType];
    auto& instructionsTo = currentVersion()->relatedInstructions[toType];
    instructionsTo.insert(instructionsFrom.begin(), instructionsFrom.end());
}

void RelaxContext::unregisterType(PirType* type) {
    if (!shouldRecord())
        return;

    currentVersion()->relatedInstructions.erase(type);
}

void RelaxContext::recordNonRefl(int pos) {
    if (!shouldRecord())
        return;

    currentVersion()->nonReflArgsQueried.insert(pos);
}

void RelaxContext::recordNotObject(const PirType* type) {
    if (!shouldRecord())
        return;

    auto& instructions = currentVersion()->relatedInstructions[type];
    for (auto i : instructions) {
        i->notObj = true;
    }
}
void RelaxContext::recordEager(const PirType* type) {
    if (!shouldRecord())
        return;

    auto& instructions = currentVersion()->relatedInstructions[type];
    for (auto i : instructions) {
        i->eager = true;
    }
}

bool RelaxContext::shouldRecord() {
    return hasCurrentVersion() && !recordingPaused;
}

void RelaxContext::recordSimpleScalar(const PirType* type) {

    if (!shouldRecord())
        return;

    auto& instructions = currentVersion()->relatedInstructions[type];
    for (auto i : instructions) {
        i->simpleScalar = true;
    }
}

ClosureVersion* RelaxContext::currentVersion() {
    assert(!recordingPaused);
    assert(!recordingStack.empty());
    return recordingStack.top();
}

bool RelaxContext::hasCurrentVersion() { return !recordingStack.empty(); }

void RelaxContext::pauseRecording() {
    assert(!recordingPaused);
    assert(!recordingStack.empty());
    recordingPaused = true;
}

void RelaxContext::resumeRecording() {
    assert(recordingPaused);
    assert(!recordingStack.empty());
    recordingPaused = false;
}

void RelaxContext::startRecording(ClosureVersion* v) {
    assert(!recordingPaused);

    recordingStack.push(v);
}

void RelaxContext::stopRecording() {
    assert(!recordingStack.empty());
    assert(!recordingPaused);

    recordingStack.pop();
}

RelaxContext* RelaxContext::singleInstance{nullptr};

} // namespace pir
} // namespace rir