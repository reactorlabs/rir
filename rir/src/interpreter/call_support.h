#pragma once

#include "R/Protect.h"
#include "runtime.h"

namespace rir {

class EnvironmentProxy;

/*
 * This class holds either a valid constructed argument list
 * (ie. a linked list of promises), or data used for delayed
 * construction of the list.
 */
class ArgumentListProxy {
  private:
    bool validArgslist_ = true;
    union {
        SEXP argslist_;
        struct {
            Code* caller;
            bool onStack;
            uint32_t id;
        } ctxt_;
    };

    void create(EnvironmentProxy* ep);

  public:
    explicit ArgumentListProxy(SEXP argslist) : argslist_{argslist} {}
    explicit ArgumentListProxy(Code* caller, bool argsOnStack, uint32_t id)
        : validArgslist_{false}, ctxt_{caller, argsOnStack, id} {}

    SEXP argslist(EnvironmentProxy* ep) {
        if (!validArgslist_)
            create(ep);
        return argslist_;
    }

    CallSite* getCallSite() {
        assert(!validArgslist_);
        return ctxt_.caller->callSite(ctxt_.id);
    }

    Code* getCaller() {
        assert(!validArgslist_);
        return ctxt_.caller;
    }

    ~ArgumentListProxy() = default;
    ArgumentListProxy(ArgumentListProxy const&) = delete;
    ArgumentListProxy(ArgumentListProxy&&) = delete;
    ArgumentListProxy& operator=(ArgumentListProxy const&) = delete;
    ArgumentListProxy& operator=(ArgumentListProxy&&) = delete;
    static void* operator new(size_t) = delete;
};

/*
 * This class holds either a valid R environment, or data to create
 * the environment lazily. In the lazy case, either we create the
 * environment at the start of evaluating RIR Code, or explicitly
 * using a special instruction.
 */
class EnvironmentProxy {
  private:
    bool validREnv_ = true;
    bool createEnvironment_ = false;
    union {
        SEXP env_;
        struct {
            SEXP call;
            SEXP callee;
            ArgumentListProxy* ap;
        } ctxt_;
    };
    EnvironmentProxy* parent_ = nullptr;

  public:
    explicit EnvironmentProxy(SEXP env) : env_{env} {
        if (!env_)
            error("'rho' cannot be C NULL: detected in C-level eval");
        if (!isEnvironment(env_))
            error(
                "'rho' must be an environment not %s: detected in C-level eval",
                type2char(TYPEOF(env_)));
    }

    explicit EnvironmentProxy(bool eager, SEXP call, SEXP callee,
                              ArgumentListProxy* ap, EnvironmentProxy* parent)
        : validREnv_{false}, createEnvironment_{eager}, ctxt_{call, callee, ap},
          parent_{parent} {}

    void init();

    SEXP env() {
        SLOWASSERT(validREnv_ && "attempt to access non-existent environment");
        return env_;
    }

    bool validREnv() { return validREnv_; }

    void set(SEXP env) {
        SLOWASSERT(env && TYPEOF(env) == ENVSXP && "setting to invalid env");
        env_ = env;
        validREnv_ = true;
    }

    void make(SEXP parent) {
        // create a new env, save it to env_ (return old?)
        assert(false && "not implemented yet");
    }

    CallSite* getCallsite() {
        assert(!validREnv_);
        return ctxt_.ap->getCallSite();
    }

    Code* getCaller() {
        assert(!validREnv_);
        return ctxt_.ap->getCaller();
    }

    SEXP getParentEnv() {
        SLOWASSERT(parent_ && parent_->validREnv_ &&
                   "Caller of PIR doesn't have valid env");
        return parent_->env();
    }

    ~EnvironmentProxy() = default;
    EnvironmentProxy(EnvironmentProxy const&) = delete;
    EnvironmentProxy(EnvironmentProxy&&) = delete;
    EnvironmentProxy& operator=(EnvironmentProxy const&) = delete;
    EnvironmentProxy& operator=(EnvironmentProxy&&) = delete;
    static void* operator new(size_t) = delete;
};

} // namespace rir
