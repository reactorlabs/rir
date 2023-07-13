#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "compiler/compiler.h"
#include "compiler/pir/builder.h"
#include "runtime/TypeFeedback.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

struct RirStack;
class MkCls;

class Rir2Pir {
  public:
    Rir2Pir(Compiler& cmp, ClosureVersion* cls, ClosureLog& log,
            const std::string& name,
            const std::list<PirTypeFeedback*>& outerFeedback,
            rir::TypeFeedback& typeFeedback);

    bool tryCompile(Builder& insert) __attribute__((warn_unused_result));
    bool tryCompileContinuation(Builder& insert, Opcode* start,
                                const std::vector<PirType>& initialStack)
        __attribute__((warn_unused_result));

    Value* tryCreateArg(rir::Code* prom, Builder& insert, bool eager)
        __attribute__((warn_unused_result));

    typedef std::unordered_map<Value*, Checkpoint*> CallTargetCheckpoints;

  private:
    Value* tryInlinePromise(rir::Code* srcCode, Builder& insert)
        __attribute__((warn_unused_result));

    // Tries to compile the srcCode. Return value indicates failure. Builder
    // has to be discarded, if compilation fails!
    bool tryCompile(rir::Code* srcCode, Builder& insert)
        __attribute__((warn_unused_result));
    bool tryCompile(rir::Code* srcCode, Builder& insert, Opcode* start,
                    const std::vector<PirType>& initialStack)
        __attribute__((warn_unused_result));
    bool tryCompilePromise(rir::Code* prom, Builder& insert)
        __attribute__((warn_unused_result));

    Value* tryTranslate(rir::Code* srcCode, Builder& insert)
        __attribute__((warn_unused_result));
    Value* tryTranslate(rir::Code* srcCode, Builder& insert, Opcode* start,
                        const std::vector<PirType>& initialStack)
        __attribute__((warn_unused_result));

    void finalize(Value*, Builder& insert);

    bool finalized = false;

    Compiler& compiler;
    ClosureVersion* cls;
    ClosureLog& log;
    std::string name;
    std::list<PirTypeFeedback*> outerFeedback;
    rir::TypeFeedback& typeFeedback;
    std::unordered_map<SEXP, MkCls*> localFuns;

    std::unordered_set<SEXP> deoptedCallTargets;
    SEXP deoptedCallReplacement = nullptr;

    struct DelayedCompilation {
        DispatchTable* dt;
        std::string name;
        SEXP formals;
        SEXP srcRef;
        bool seen;
        Context context;
    };
    std::unordered_map<MkCls*, DelayedCompilation> delayedCompilation;

    bool compileBC(const BC& bc, Opcode* pos, Opcode* nextPos,
                   rir::Code* srcCode, RirStack&, Builder&,
                   CallTargetCheckpoints&);
    virtual bool inPromise() const { return false; }
    virtual bool inlining() const { return false; }

    Checkpoint* addCheckpoint(rir::Code* srcCode, Opcode* pos,
                              const RirStack& stack, Builder& insert) const;
};

class PromiseRir2Pir : public Rir2Pir {
  public:
    PromiseRir2Pir(Compiler& cmp, ClosureVersion* cls, ClosureLog& log,
                   const std::string& name,
                   const std::list<PirTypeFeedback*>& outerFeedback,
                   rir::TypeFeedback& feedback, bool inlining)
        : Rir2Pir(cmp, cls, log, name, outerFeedback, feedback),
          inlining_(inlining) {}

  private:
    bool inlining_;
    bool inlining() const override final { return inlining_; }
    bool inPromise() const override final { return true; }
};

} // namespace pir
} // namespace rir

#endif
