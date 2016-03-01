#ifndef ANALYSIS_TYPEANDSHAPE_H
#define ANALYSIS_TYPEANDSHAPE_H

#include "ir/Ir.h"
#include "ir/Pass.h"
#include "ir/PassDriver.h"

#include "TypeInfo.h"
#include "Instrumentation.h"
#include "Flags.h"

namespace rjit {
namespace analysis {

class TypeAndShapePass : public ir::Fixpoint<ir::AState<TypeInfo>> {
  public:
    typedef TypeInfo Value;
    typedef ir::AState<Value> State;

    void constantLoad(SEXP c, ir::Value p) { state[p] = TypeInfo(c); }

    match constant(ir::UserLiteral* p) { constantLoad(p->indexValue(), p); }

    match binops(ir::BinaryOperator* p) {
        auto lhs = p->lhs();
        auto rhs = p->rhs();
        auto l = state[lhs];
        auto r = state[rhs];
        state[p->pattern()] = Value::merge(l, r);
        // state[p->pattern()] = Value::merge(state[lhs], state[rhs]);
    }

    /** If we have information about the variable, store it to the register,
     * otherwise initialize the register to top.
       */
    match genericGetVar(ir::GenericGetVar* p) {
        llvm::Value* dest = p->result();
        SEXP symbol = p->symbolValue();
        if (state.has(symbol)) {
            state[dest] = state[symbol];
            return;
        }
        if (Flag::singleton().useTypefeedback && typeFeedback) {
            TypeInfo inf = typeFeedback->get(symbol);

            // We cannot guard integer overflow to NA yet:
            if (!Flag::singleton().unsafeNA &&
                (inf.hasOnlyType(TypeInfo::Type::Integer) ||
                 inf.hasOnlyType(TypeInfo::Type::Bool))) {
                inf.addType(TypeInfo::Type::Any);
            }

            state[dest] = inf;
            return;
        }

        state[dest] = Value::any();
    }

    /** If we have incomming type & shape information, store it in the variable
     * too. Otherwise do nothing (this means the variable will be assumed Top at
     * read).
     */
    match genericSetVar(ir::GenericSetVar* p) {
        llvm::Value* src = p->value();
        SEXP symbol = p->symbolValue();
        if (state.has(src))
            state[symbol] = state[src];
    }

    /** When creating the scallar, the result is a scalar of the element's
     * appropriate type.
     */
    match scalarAlloc(ir::CreateAndSetScalar* p) {
        if (p->scalarType() == t::Double) {
            state[p] = Value(Value::Type::Float, Value::Size::Scalar,
                             Value::Attrib::Absent);
        } else {
            assert(p->scalarType() == t::Int and
                   "Only integers and doubles are supported");
            state[p] = Value(Value::Type::Integer, Value::Size::Scalar,
                             Value::Attrib::Absent);
        }
    }

    /** A call to ICStub invalidates all variables.
     */
    match call(ir::ICStub* ins) { state.invalidateVariables(Value::any()); }

    bool dispatch(llvm::BasicBlock::iterator& i) override;
};

class TypeAndShape : public ir::ForwardDriver<TypeAndShapePass> {
  public:
    typedef TypeInfo Value;
    typedef ir::AState<Value> State;
};

} // namespace analysis
} // namespace rjit

#endif // IR_ANALYSIS_TYPEANDSHAPE_H
