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
    Rir2Pir(Rir2PirCompiler& cmp, rir::Function* srcFunction, LogStream& log,
            const std::string& name)
        : compiler(cmp), srcFunction(srcFunction), log(log), name(name) {}

    bool tryCompile(Builder& insert) __attribute__((warn_unused_result)) {
        return tryCompile(srcFunction->body(), insert);
    }

  private:
    // Tries to compile the srcCode. Return value indicates failure. Builder
    // has to be discarded, if compilation fails!
    bool tryCompile(rir::Code* srcCode, Builder& insert)
        __attribute__((warn_unused_result));
    bool tryCompilePromise(rir::Code* prom, Builder& insert) const
        __attribute__((warn_unused_result));

    Value* tryTranslate(rir::Code* srcCode, Builder& insert) const
        __attribute__((warn_unused_result));

    void finalize(Value*, Builder& insert);

    bool finalized = false;

    Rir2PirCompiler& compiler;
    rir::Function* srcFunction;
    LogStream& log;
    std::string name;

    bool compileBC(const BC& bc, Opcode* pos, rir::Code* srcCode, RirStack&,
                   Builder&) const;
    virtual bool inPromise() const { return false; }
};

class PromiseRir2Pir : public Rir2Pir {
  public:
    PromiseRir2Pir(Rir2PirCompiler& cmp, rir::Function* srcFunction,
                   LogStream& log, const std::string& name)
        : Rir2Pir(cmp, srcFunction, log, name) {}

  private:
    bool inPromise() const override final { return true; }
};

} // namespace pir
} // namespace rir

#endif
