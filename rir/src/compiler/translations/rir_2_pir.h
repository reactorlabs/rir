#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "../pir/stack_machine.h"
#include "pir_translator.h"
#include "rir_2_pir_compiler.h"
#include <unordered_map>

namespace rir {
namespace pir {

class Rir2Pir {
  public:
    Rir2Pir(Rir2PirCompiler& cmp, Builder& insert, rir::Function* srcFunction,
            rir::Code* srcCode)
        : insert(insert), cmp(cmp), srcFunction(srcFunction), srcCode(srcCode) {}

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
