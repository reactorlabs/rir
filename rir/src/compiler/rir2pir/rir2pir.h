#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "compiler/compiler.h"
#include "compiler/pir/builder.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

struct RirStack;
class MkFunCls;

class Rir2Pir {
  public:
    Rir2Pir(Compiler& cmp, ClosureVersion* cls, ClosureStreamLogger& log,
            const std::string& name,
            const std::list<PirTypeFeedback*>& outerFeedback);

    bool tryCompile(Builder& insert) __attribute__((warn_unused_result));

    Value* tryCreateArg(rir::Code* prom, Builder& insert, bool eager)
        __attribute__((warn_unused_result));

  private:
    Value* tryInlinePromise(rir::Code* srcCode, Builder& insert)
        __attribute__((warn_unused_result));

    // Tries to compile the srcCode. Return value indicates failure. Builder
    // has to be discarded, if compilation fails!
    bool tryCompile(rir::Code* srcCode, Builder& insert)
        __attribute__((warn_unused_result));
    bool tryCompilePromise(rir::Code* prom, Builder& insert)
        __attribute__((warn_unused_result));

    Value* tryTranslate(rir::Code* srcCode, Builder& insert)
        __attribute__((warn_unused_result));

    void finalize(Value*, Builder& insert);

    bool finalized = false;

    Compiler& compiler;
    ClosureVersion* cls;
    ClosureStreamLogger& log;
    std::string name;
    std::list<PirTypeFeedback*> outerFeedback;

    struct DelayedCompilation {
        DispatchTable* dt;
        std::string name;
        SEXP formals;
        SEXP srcRef;
        bool seen;
        Context context;
    };
    std::unordered_map<MkFunCls*, DelayedCompilation> delayedCompilation;

    typedef std::unordered_map<
        Value*, std::tuple<Checkpoint*, ObservedCallees, Opcode*>>
        CallTargetFeedback;

    bool compileBC(const BC& bc, Opcode* pos, Opcode* nextPos,
                   rir::Code* srcCode, RirStack&, Builder&,
                   CallTargetFeedback&);
    virtual bool inPromise() const { return false; }
    virtual bool inlining() const { return false; }

    Checkpoint* addCheckpoint(rir::Code* srcCode, Opcode* pos,
                              const RirStack& stack, Builder& insert) const;
};

class PromiseRir2Pir : public Rir2Pir {
  public:
    PromiseRir2Pir(Compiler& cmp, ClosureVersion* cls, ClosureStreamLogger& log,
                   const std::string& name,
                   const std::list<PirTypeFeedback*>& outerFeedback,
                   bool inlining)
        : Rir2Pir(cmp, cls, log, name, outerFeedback), inlining_(inlining) {}

  private:
    bool inlining_;
    bool inlining() const override final { return inlining_; }
    bool inPromise() const override final { return true; }
};

} // namespace pir
} // namespace rir

#endif
