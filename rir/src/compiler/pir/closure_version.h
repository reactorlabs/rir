#ifndef COMPILER_CLOSURE_VERSION_H
#define COMPILER_CLOSURE_VERSION_H

#include "code.h"
#include "compiler/log/debug.h"
#include "pir.h"
#include "report.h"
#include "runtime/Function.h"
#include <functional>
#include <sstream>
#include <unordered_map>

namespace rir {
namespace pir {

/*
 * ClosureVersion
 *
 */
class ClosureVersion : public Code {
  public:
    enum class Property {
        IsEager,
        NoReflection,

        FIRST = IsEager,
        LAST = NoReflection

    };

    struct Properties : public EnumSet<Property> {
        Properties() : EnumSet<Property>() {}
        explicit Properties(const EnumSet<Property>& other)
            : EnumSet<Property>(other) {}
        explicit Properties(const Property& other) : EnumSet<Property>(other) {}

        std::vector<size_t> argumentForceOrder;
        friend std::ostream& operator<<(std::ostream& out, const Properties&);
    };

    size_t inlinees = 0;

    const bool root;

    // these two variables are used by the scheduler algorithm
    // in order to avoid running the optimization passes to versions
    // that have already converged (compiler.cpp)
    // The reasons these are set to true is so that versions that are
    // cloned in mid compilation (see references to cloneWithCurrentContext)
    // are also included in optimizations
    bool anyChangePreviousIter = true;
    bool anyChangeCurrentIter = true;

    rir::Function* optFunction;

    // FB
    std::unordered_map<Function*, report::FeedbackStatsPerFunction>
        feedbackStatsByFunction;

    report::FeedbackStatsPerFunction& feedbackStatsFor(Function* baseline) {
        return feedbackStatsByFunction[baseline];
    }

    ClosureVersion* getClosureVersion() override {
        return this;
    }

  private:
    Closure* owner_;
    std::vector<Promise*> promises_;
    const Context optimizationContext_;

    std::string name_;
    std::string nameSuffix_;

  protected:
    ClosureVersion(Closure* closure, rir::Function* optFunction, bool root,
                   const Context& optimizationContext,
                   const Properties& properties = Properties());

    friend class Closure;

  public:
    void computeFeedbackStats();
    void scanForSpeculation();
    void scanForPreciseTypeSlots();
    void computeSlotsPresent();

    ClosureVersion* clone(const Context& newContext);

    const Context& context() const { return optimizationContext_; }

    Properties properties;

    Closure* owner() const { return owner_; }
    size_t nargs() const;
    size_t effectiveNArgs() const;

    const std::string& name() const { return name_; }
    const std::string& nameSuffix() const { return nameSuffix_; }

    void printName(std::ostream& out) const override { out << name(); }

    void print(std::ostream& out, bool tty) const;
    void print(DebugStyle style, std::ostream& out, bool tty,
               bool omitDeoptBranches) const;
    void printStandard(std::ostream& out, bool tty,
                       bool omitDeoptBranches) const;
    void printGraph(std::ostream& out, bool omitDeoptBranches) const;
    void printBBGraph(std::ostream& out, bool omitDeoptBranches) const;

    Promise* createProm(rir::Code* rirSrc);

    Promise* promise(unsigned id) const { return promises_.at(id); }
    const std::vector<Promise*>& promises() { return promises_; }

    void erasePromise(unsigned id);

    typedef std::function<void(Promise*)> PromiseIterator;
    void eachPromise(PromiseIterator it) const {
        for (auto p : promises_)
            if (p)
                it(p);
    }

    size_t numNonDeoptInstrs() const;

    rir::Code* rirSrc() const override final;

    virtual Continuation* isContinuation() { return nullptr; }

    ~ClosureVersion();
};

std::ostream& operator<<(std::ostream& out, const ClosureVersion::Property&);

} // namespace pir
} // namespace rir

#endif
