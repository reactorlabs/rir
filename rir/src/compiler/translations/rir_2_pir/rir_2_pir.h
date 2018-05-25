#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "../pir_translator.h"
#include "rir_2_pir_compiler.h"
#include "stack_machine.h"
#include <unordered_map>

namespace rir {
namespace pir {

class Rir2Pir {
  public:
    Rir2Pir(Rir2PirCompiler& cmp, rir::Function* srcFunction)
        : compiler(cmp), srcFunction(srcFunction) {}
    Rir2Pir(const Rir2Pir& r2p) : Rir2Pir(r2p.compiler, r2p.srcFunction) {}

    void compile(rir::Code* srcCode, Builder& insert) {
        finalize(translate(srcCode, insert), insert);
    }

    ~Rir2Pir() { assert(finalized); }

    static bool supported(rir::Function* fun);

  private:
    Value* translate(rir::Code* srcCode, Builder& insert) const;
    void finalize(Value*, Builder& insert);

    bool finalized = false;

    Rir2PirCompiler& compiler;
    rir::Function* srcFunction;

    bool doMerge(Opcode* trg);

    friend class StackMachine;
};
} // namespace pir
} // namespace rir
#endif
