#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "../ir_translator.h"
#include "../rir_compiler.h"
#include "stack_machine.h"
#include <unordered_map>

namespace rir {
namespace pir {

class Rir2Pir : public IRTranslator {
  public:
    Rir2Pir(RirCompiler& cmp) : Rir2Pir(cmp, "Rir 2 Pir") {}
    Rir2Pir(RirCompiler& cmp, std::string name) : IRTranslator(cmp, name) {}

    void apply(IRCode);
    Value* translate(rir::Function*, rir::Code*);

    typedef StackMachine::ReturnSite ReturnSite;
    void addReturn(ReturnSite r) { results.push_back(r); }

  private:
    bool done = false;

    std::unordered_map<Opcode*, StackMachine> mergepoint;
    std::vector<ReturnSite> results;

    void recoverCFG(rir::Function*, rir::Code*);
    bool doMerge(Opcode* trg);
    virtual void compileReturn(Value*);

    friend class RirInlinedPromise2Rir;
};
} // namespace pir
} // namespace rir
#endif
