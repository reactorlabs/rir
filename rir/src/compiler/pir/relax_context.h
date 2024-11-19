#ifndef COMPILER_RELAX_CONTEXT_H
#define COMPILER_RELAX_CONTEXT_H

#include <map>
#include <stack>
#include <unordered_set>

#include "pir.h"

namespace rir {
namespace pir {

class LdArg;
class PirType;

class RelaxContext {
  public:
    RelaxContext() {}

    void unregisterType(PirType* type);
    void recordNotObject(const PirType* type);
    void recordEager(const PirType* type);

    void recordSimpleScalar(const PirType* type);

    void recordNonRefl(int pos);

    void startRecording(ClosureVersion* v);
    void stopRecording();

    void copyInfo(const PirType* fromType, const PirType* toType);

    ClosureVersion* currentVersion();
    bool hasCurrentVersion();

    void pauseRecording();
    void resumeRecording();
    bool shouldRecord();

    static RelaxContext& singleton() {
        if (!singleInstance)
            singleInstance = new RelaxContext();

        return *singleInstance;
    }

  private:
    bool recordingPaused = false;
    std::stack<ClosureVersion*> recordingStack;

    static RelaxContext* singleInstance;
};

} // namespace pir
} // namespace rir

#endif
