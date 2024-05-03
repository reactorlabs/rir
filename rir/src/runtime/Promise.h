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
    // extra pool
    static constexpr size_t NUM_PTRS = 1;
    // 0: promise code (Code*)
    static constexpr size_t PROMISE_CODE_IDX = 0;

    const CallContext* context_;
    SEXP locals_[NUM_PTRS];
  public:
    Promise(const CallContext* context, Code* code)
        : RirRuntimeObject(
              // GC area starts at &locals and goes to the end of defaultArg_
              (intptr_t)&locals_ - (intptr_t)this, NUM_PTRS),
          context_(context)
    //          context_(context->callId, context->caller, context->callee,
    //          context->passedArgs, context->ast, context->stackArgs,
    //          context->names, context->callerEnv,
    //          context->suppliedvars, context->givenContext)
    {
        setEntry(PROMISE_CODE_IDX, code->container());
    }
    ~Promise() = default;

    static Promise* create(const CallContext* context, Code* code) {
        size_t sz = sizeof(Promise);
        SEXP s = Rf_allocVector(EXTERNALSXP, sz);
        return new (INTEGER(s)) Promise(context, code);
    }

    const CallContext* context() const { return context_; }

    Code* code() const { return Code::unpack(getEntry(PROMISE_CODE_IDX)); }

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
