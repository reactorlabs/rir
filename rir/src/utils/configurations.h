#ifndef RIR_CONFIGURATIONS_H
#define RIR_CONFIGURATIONS_H

#include "compiler/translations/pir_translator.h"
#include "utils/INIReader.h"

#include <set>

namespace rir {

class Configurations {
  public:
    Configurations() { parseINIFile(); }
    const std::vector<const pir::PirTranslator*>& pirOptimizations() {
        return optimizations;
    }
    ~Configurations() {
        for (auto o : optimizations)
            delete o;
    }

  private:
    std::vector<const pir::PirTranslator*> optimizations;
    void defaultOptimizations();
    void parseINIFile();
};

} // namespace rir
#endif
