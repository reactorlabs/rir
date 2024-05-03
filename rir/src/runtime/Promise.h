#ifndef RIR_PROMISE_H
#define RIR_PROMISE_H

#include "Code.h"
#include "RirRuntimeObject.h"
#include "interpreter/call_context.h"

#include <cassert>
#include <cstdint>
#include <ostream>
#include <time.h>

#include <asm/msr.h>

namespace rir {

#define PROMISE_MAGIC 0xaaa0f13e

/**
 * Promise class couples promise Code with CallContext
 * under which promise was created.
 */
#pragma pack(push)
#pragma pack(1)

class Promise : public RirRuntimeObject<Promise, PROMISE_MAGIC> {

    const CallContext* context_;
    SEXP code_;

  public:
    Promise(const CallContext* context, Code* code)
        : RirRuntimeObject(
              // GC area starts at &locals and goes to the end of defaultArg_
              (intptr_t)&code_ - (intptr_t)this, 1),
          context_(context), code_(nullptr)
    //          context_(context->callId, context->caller, context->callee,
    //          context->passedArgs, context->ast, context->stackArgs,
    //          context->names, context->callerEnv,
    //          context->suppliedvars, context->givenContext)
    {
        setEntry(0, code->container());
    }
    ~Promise() = default;

    static Promise* create(const CallContext* context, Code* code) {
        size_t sz = sizeof(Promise);
        SEXP s = Rf_allocVector(EXTERNALSXP, sz);
        return new (INTEGER(s)) Promise(context, code);
    }

    const CallContext* context() const { return context_; }

    Code* code() const { return Code::unpack(getEntry(0)); }

    // Serialize and deserialize only code for now
    void serialize(SEXP refTable, R_outpstream_t out) const {
        code()->serialize(refTable, out);
    }

    static Promise* deserialize(SEXP refTable, R_inpstream_t inp) {
        Code* code = Code::deserialize(refTable, inp);
        return create(nullptr, code);
    }
};

#pragma pack(pop)

} // namespace rir

#endif
