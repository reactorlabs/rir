#ifndef COMPILER_AA_H
#define COMPILER_AA_H

#include <map>
#include <unordered_set>

//#include "pir.h"

namespace rir {
namespace pir {

class LdArg;
class PirType;

class AA {
  public:
    AA() {}

    void notMaybeObject(PirType* type);
    void setCurrentVersion(ClosureVersion* v);

    static AA& singleton() {
        if (!singleInstance)
            singleInstance = new AA();

        return *singleInstance;
    }

  private:
    ClosureVersion* currentVersion = nullptr;
    static AA* singleInstance;
};

} // namespace pir
} // namespace rir

#endif
