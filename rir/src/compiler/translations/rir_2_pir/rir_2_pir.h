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

    typedef std::function<void()> Maybe;
    typedef std::function<void(Value*)> MaybeVal;

    bool tryCompile(rir::Code* srcCode, Builder& insert)
        __attribute__((warn_unused_result)) {
        return translate<bool>(srcCode, insert,
                               [&](Value* v) {
                                   finalize(v, insert);
                                   return true;
                               },
                               []() { return false; });
    }

  private:
    void translate(rir::Code* srcCode, Builder& insert, MaybeVal success,
                   Maybe fail) const;

    void translate(rir::Code* srcCode, Builder& insert,
                   MaybeVal success) const {
        translate(srcCode, insert, success, []() {});
    }

    template <typename T>
    T translate(rir::Code* srcCode, Builder& insert,
                std::function<T(Value*)> success,
                std::function<T()> fail) const {
        T res;
        translate(srcCode, insert, [&](Value* v) { res = success(v); },
                  [&]() { res = fail(); });
        return res;
    }

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
