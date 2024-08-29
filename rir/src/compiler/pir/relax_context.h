#ifndef COMPILER_RELAX_CONTEXT_H
#define COMPILER_RELAX_CONTEXT_H

#include <map>
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

    void setCurrentVersion(ClosureVersion* v);
    void copyInfo(const PirType* fromType, const PirType* toType);

    static RelaxContext& singleton() {
        if (!singleInstance)
            singleInstance = new RelaxContext();

        return *singleInstance;
    }

  private:
    ClosureVersion* currentVersion = nullptr;
    static RelaxContext* singleInstance;
};

} // namespace pir
} // namespace rir

#endif
