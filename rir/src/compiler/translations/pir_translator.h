#ifndef PIR_TRANSLATOR_H
#define PIR_TRANSLATOR_H

#include "../pir/module.h"
#include "../pir/value.h"
#include "runtime/Function.h"

namespace rir {

class PirTranslator {
  public:
    pir::Module* module;
    virtual pir::Function* compileFunction(SEXP) = 0;
    //virtual pir::Function* declare(const std::vector<SEXP>& a) = 0;
    PirTranslator() : module(new pir::Module) , verbose(false) {}
    PirTranslator(pir::Module* mod) : module(mod), verbose(false) {}

    bool getVerbose();
    void setVerbose(bool);
    pir::Module* getModule();
  private:
      bool verbose;
};
}

#endif