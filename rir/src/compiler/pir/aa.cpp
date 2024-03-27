#include "aa.h"

#include "pir_impl.h"

namespace rir {
namespace pir {

void AA::setNotObj() {

    if (currentVersion) {
        currentVersion->notObjUsed = true;
    }
}

void AA::setCurrentVersion(ClosureVersion* v) { currentVersion = v; }

AA* AA::singleInstance{nullptr};

} // namespace pir
} // namespace rir