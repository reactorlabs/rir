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
        auto T = [&](const Optimization& o) {
            return std::tuple<unsigned, unsigned, string>(
                o.order, o.translator->getName() == "cleanup" ? 0 : 1,
                o.translator->getName());
        };
        return T(*this) < T(anotherOptimization);
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