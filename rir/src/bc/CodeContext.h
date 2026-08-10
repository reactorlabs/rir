#ifndef RIR_BC_CODE_CONTEXT_H
#define RIR_BC_CODE_CONTEXT_H

#include "R/r.h"
#include "bc/BC.h"
#include "bc/CodeStream.h"
#include "bc/DefUseAnalysis.h"
#include "interpreter/cache.h"

#include <cstdint>
#include <stack>
#include <unordered_map>
#include <vector>

namespace rir {

struct LoopContext {
    BC::Label next_;
    BC::Label break_;
    bool context_needed_ = false;
    LoopContext(BC::Label next_, BC::Label break_)
        : next_(next_), break_(break_) {}
};

class CodeContext {
  public:
    typedef size_t CacheSlotNumber;
    static constexpr CacheSlotNumber BindingCacheDisabled = (size_t)-1;

    CodeStream cs;
    std::stack<LoopContext> loops;
    CodeContext* parent;
    std::unordered_map<SEXP, CacheSlotNumber> loadsSlotInCache;

    CodeContext(SEXP ast, FunctionWriter& fun, CodeContext* p,
                DefUseAnalysis dua = {})
        : cs(fun, ast), parent(p), defUseAnalysis(std::move(dua)) {}
    virtual ~CodeContext() {}
    bool inLoop() { return !loops.empty() || (parent && parent->inLoop()); }
    BC::Label loopNext() {
        assert(!loops.empty());
        return loops.top().next_;
    }
    BC::Label loopBreak() {
        assert(!loops.empty());
        return loops.top().break_;
    }
    void setContextNeeded() {
        if (loops.empty() && parent)
            parent->setContextNeeded();
        else
            loops.top().context_needed_ = true;
    }
    size_t isCached(SEXP name) {
        assert(loadsSlotInCache.size() <= MAX_CACHE_SIZE);
        auto f = loadsSlotInCache.find(name);
        return f != loadsSlotInCache.end() && f->second != BindingCacheDisabled;
    }
    size_t nCached = 0;
    size_t cacheSlotFor(SEXP name) {
        auto f = loadsSlotInCache.find(name);
        if (f != loadsSlotInCache.end())
            return f->second;
        if (nCached >= MAX_CACHE_SIZE)
            return BindingCacheDisabled;
        return loadsSlotInCache.emplace(name, nCached++).first->second;
    }
    virtual bool loopIsLocal() { return !loops.empty(); }
    virtual bool isPromiseContext() { return false; }

    DefUseAnalysis defUseAnalysis;
    uint32_t recordTypeOnceBitmapSize = 0;

    // Expression-tree construction state: the type-feedback slots recorded so
    // far at each open LANGSXP nesting level, innermost on top. Pushed/popped
    // exclusively by compileExpr's LANGSXP case, so it is empty again when this
    // Code object is done (asserted in CompilerContext::pop()).
    //
    // Deliberately per Code object rather than per function: an expression tree
    // never spans a Code boundary. A promise body is a separate Code object
    // whose evaluation is decoupled in time from the expression that created
    // it, so its operands are not operands of that expression. Holding this on
    // the (function-wide) CompilerContext used to let a promise's leaves land
    // in whatever level the *enclosing* function had open and be adopted by an
    // unrelated node — e.g. in `f(x) + g(x)` the promise loads of `x` became
    // children of the `+`. See recordless-design.md §2C.5.
    std::stack<std::vector<uint32_t>> slotsStack;
};

class PromiseContext : public CodeContext {
  public:
    PromiseContext(SEXP ast, FunctionWriter& fun, CodeContext* p,
                   DefUseAnalysis dua = {})
        : CodeContext(ast, fun, p, std::move(dua)) {}
    bool loopIsLocal() override {
        if (loops.empty()) {
            parent->setContextNeeded();
            return false;
        }
        return true;
    }
    bool isPromiseContext() override { return true; }
};

} // namespace rir

#endif // RIR_BC_CODE_CONTEXT_H
