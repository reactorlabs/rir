#ifndef COMPILER_AA_H
#define COMPILER_AA_H

//#include "pir.h"

namespace rir {
namespace pir {

class ClosureVersion;

class AA {
  public:
    AA() { currentVersion = nullptr; }

    void setNotObj();
    void setCurrentVersion(ClosureVersion* v);

    static AA& singleton() {
        if (!singleInstance)
            singleInstance = new AA();

        return *singleInstance;
    }

  private:
    ClosureVersion* currentVersion;
    static AA* singleInstance;
};

} // namespace pir
} // namespace rir

#endif
