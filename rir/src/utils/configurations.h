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
};

struct OptmizationCmp {
    bool operator()(const Optimization* opt,
                    const Optimization* anotherOpt) const {
        auto T = [&](const Optimization* o) {
            return std::tuple<unsigned, unsigned, string>(
                o->order, o->translator->getName() == "cleanup" ? 1 : 0,
                o->translator->getName());
        };
        return T(opt) < T(anotherOpt);
    }
};

class Configurations {
  public:
    Configurations() { this->parseINIFile(); }
    std::multiset<Optimization*, OptmizationCmp>& pirOptimizations() {
        return optimizations;
    }
    ~Configurations() { optimizations.clear(); }

  private:
    std::multiset<Optimization*, OptmizationCmp> optimizations;
    void defaultOptimizations();
    void read(INIReader&, std::string);
    void parseINIFile();
};

} // namespace rir
#endif