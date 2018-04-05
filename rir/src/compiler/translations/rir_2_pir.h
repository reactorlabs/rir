#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "pir_translator.h"
#include "../pir/stack_machine.h"
#include <unordered_map>

namespace rir {
namespace pir {

class Rir2PirCompiler {
  public:
    Rir2PirCompiler(Module* module) : module(module) {}
    Closure* compileClosure(SEXP);
    Closure* compileClosure(rir::Function*, const std::vector<SEXP>&,
                            Value* closureEnv);
    Closure* compileFunction(rir::Function*, const std::vector<SEXP>&);
    void optimizeModule();
    Module* getModule() { return module; }

    bool isVerbose() { return verbose; }
    void setVerbose(bool v) { verbose = v; }

  private:
    bool verbose = false;
    Module* module;
};

class Rir2Pir : public PirTranslator {
  public:
    Rir2Pir(Rir2PirCompiler& cmp, Builder& insert, rir::Function* srcFunction,
            rir::Code* srcCode)
        : PirTranslator(cmp.isVerbose()), insert(insert), cmp(cmp),
          srcFunction(srcFunction), srcCode(srcCode) {}

    Value* translate();

    typedef StackMachine::ReturnSite ReturnSite;
    void addReturn(ReturnSite r) { results.push_back(r); }

    Rir2PirCompiler& compiler() { return cmp; }

  private:
    bool done = false;

    Builder& insert;
    Rir2PirCompiler& cmp;

    rir::Function* srcFunction;
    rir::Code* srcCode;

    std::unordered_map<Opcode*, StackMachine> mergepoint;
    std::vector<ReturnSite> results;

    void recoverCFG(rir::Code*);
    bool doMerge(Opcode* trg);
    virtual void compileReturn(Value*);

    friend class RirInlinedPromise2Rir;
};
}
}
#endif
