#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "../../util/builder.h"
#include "../pir_translator.h"
#include "rir_2_pir_compiler.h"

#include <unordered_set>

namespace rir {
namespace pir {

struct RirStack;

class Rir2Pir {
  public:
    Rir2Pir(Rir2PirCompiler& cmp, rir::Function* srcFunction)
        : compiler(cmp), srcFunction(srcFunction) {}

    // Tries to compile the srcCode. Return value indicates failure. Builder
    // has to be discarded, if compilation fails!
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
    bool tryCompilePromise(rir::Code* prom, Builder& insert) const
        __attribute__((warn_unused_result)) {
        return Rir2Pir(compiler, srcFunction).tryCompile(prom, insert);
    }

    template <typename T>
    using Maybe = std::function<T()>;
    template <typename T>
    using MaybeVal = std::function<T(Value*)>;

    // Try to translate. On success and on fail "callbacks" are passed
    void translate(rir::Code* srcCode, Builder& insert, MaybeVal<void> success,
                   Maybe<void> fail) const;

    // Try to translate. On fail is silent.
    void translate(rir::Code* srcCode, Builder& insert,
                   MaybeVal<void> success) const {
        translate(srcCode, insert, success, []() {});
    }

    // Try to translate. Either fail or success is invoked. The final result is
    // the return value of either of those.
    template <typename T>
    T translate(rir::Code* srcCode, Builder& insert, MaybeVal<T> success,
                Maybe<T> fail) const {
        T res;
        translate(srcCode, insert, [&](Value* v) { res = success(v); },
                  [&]() { res = fail(); });
        return res;
    }

    void finalize(Value*, Builder& insert);

    bool finalized = false;

    Rir2PirCompiler& compiler;
    rir::Function* srcFunction;

    bool compileBC(BC bc, Opcode* pos, rir::Code* srcCode, RirStack&, Builder&,
                   std::unordered_map<Value*, CallFeedback>&,
                   std::array<TypeFeedback, 2>&) const;
};

} // namespace pir
} // namespace rir

#endif
