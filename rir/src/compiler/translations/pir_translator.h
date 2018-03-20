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
    virtual pir::Function* compileFunction(rir::Function*) = 0;
    virtual pir::Function* compileFunction(pir::IRTransformation*) = 0;
    pir::Function* declare(const std::vector<SEXP>& a);
    PirTranslator() : module(new pir::Module) , verbose(false) {}

    bool getVerbose();
    void setVerbose(bool);
    pir::Module* getModule();
  private:
      pir::Module* compileModule();
      bool verbose;
};
}

#endif