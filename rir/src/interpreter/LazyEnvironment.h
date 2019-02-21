#ifndef LAZY_ENVIRONMENT_H
#define LAZY_ENVIRONMENT_H

#include "../ir/BC_inc.h"
#include "RirDataWrapper.h"
#include "interp_incl.h"

#include <cassert>
#include <cstdint>
#include <functional>

namespace rir {

#define LAZY_ENVIRONMENT_MAGIC 0xe4210e47

/**
 * EnvironmentStub holds the information needed to create an
 * environment lazily.
 */
struct LazyEnvironment
    : public RirDataWrapper<LazyEnvironment, LAZY_ENVIRONMENT_MAGIC> {
    LazyEnvironment() = delete;
    LazyEnvironment(const LazyEnvironment&) = delete;
    LazyEnvironment& operator=(const LazyEnvironment&) = delete;

    LazyEnvironment(std::vector<SEXP>* arguments, SEXP parent, Opcode* opcode,
                    InterpreterInstance* cmpCtx, R_bcstack_t* localsBase)
        : RirDataWrapper(5, arguments->size()), arguments_(arguments),
          parent_(parent), names_(opcode), cmpCtx_(cmpCtx),
          localsBase_(localsBase){};
    ~LazyEnvironment() { delete arguments_; }

    std::vector<SEXP>* arguments() { return arguments_; }
    const SEXP parent() { return parent_; }
    const Opcode* names() { return names_; }
    InterpreterInstance* cmpCtx() { return cmpCtx_; }
    R_bcstack_t* localsBase() { return localsBase_; }

    SEXP create() {
        return createEnvironment(arguments(), parent(), names(), cmpCtx(),
                                 localsBase(), (SEXP)this);
    }

    SEXP* gcData() { return arguments_->data(); }

  private:
    std::vector<SEXP>* arguments_;
    const SEXP parent_;
    const Opcode* names_;
    InterpreterInstance* cmpCtx_;
    R_bcstack_t* localsBase_;
};
} // namespace rir

#endif
