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
            return anotherOptimization.translator->getName() == "cleanup";
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