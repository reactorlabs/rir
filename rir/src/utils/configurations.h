#ifndef RIR_CONFIGURATIONS_H
#define RIR_CONFIGURATIONS_H

#include "compiler/translations/pir_translator.h"
#include "utils/INIReader.h"

namespace rir {

struct Optimization {
    Optimization(pir::PirTranslator* translator, short order)
        : translator(translator), order(order) {}
    ~Optimization() { delete translator; }

    pir::PirTranslator* translator;
    short order;

    bool operator<(const Optimization& anotherOptimization) const {
        if (order == anotherOptimization.order) {
            if (anotherOptimization.translator->getName() == "cleanup") {
                return true;
            } else if (this->translator->getName() == "cleanup") {
                return false;
            } else {
                return anotherOptimization.translator->getName().compare(
                           this->translator->getName()) > 0;
            }
        } else {
            return order < anotherOptimization.order;
        }
    }
};

class Configurations {
  public:
    Configurations() { this->parseINIFile(); }
    std::set<Optimization*>& pirOptimizations() { return optimizations; }
    ~Configurations() { optimizations.clear(); }

  private:
    std::set<Optimization*> optimizations;
    void defaultOptimizations();
    void read(INIReader&, std::string);
    void parseINIFile();
};

} // namespace rir
#endif