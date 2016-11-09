#ifndef RIR_ANALYSIS_DATAFLOW_H
#define RIR_ANALYSIS_DATAFLOW_H

#include "ir/CodeEditor.h"
#include "R/Funtab.h"
#include "code/analysis.h"
#include "code/dispatchers.h"
#include "interpreter/interp_context.h"

namespace rir {

/*
 *                       Maybe  (might be deleted from env)
 *
 *          Absent             Any (prom)
 *
 *                      Value         Argument
 *
 *                    Constant
 */

class FValue {
  public:
    enum class Type { Bottom, Absent, Constant, Argument, Value, Any, Maybe };

  private:
    FValue(Type t, int argument) : t(t), argument(argument) {}

    FValue(Type t, CodeEditor::Iterator def = UseDef::unused())
        : t(t), u(def) {}

  public:
    Type t = Type::Bottom;

    FValue() {}
    FValue(const FValue& p) : t(p.t), argument(p.argument), u(p.u) {}

    static FValue Argument(int i) { return FValue(Type::Argument, i); }
    static FValue Constant(CodeEditor::Iterator def) {
        return FValue(Type::Constant, def);
    }
    static FValue Maybe() { return FValue(Type::Maybe); }
    static FValue Any(CodeEditor::Iterator ins = UseDef::multiuse()) {
        return FValue(Type::Any, ins);
    }
    static FValue Value(CodeEditor::Iterator ins) {
        return FValue(Type::Value, ins);
    }
    static FValue Absent() { return FValue(Type::Absent); }
    static FValue Bottom() { return FValue(Type::Bottom); }

    FValue duplicate(CodeEditor::Iterator pos) {
        FValue d;
        d.t = t;
        d.u = UseDef();
        d.u.def = pos;
        return d;
    }

    bool singleDef() const {
        if (hasUseDef()) {
            assert(u.def != UseDef::unused());
            return u.def != UseDef::multiuse();
        }
        return false;
    }

    CodeEditor::Iterator def() {
        assert(singleDef());
        return u.def;
    }

    void used(CodeEditor::Iterator ins) {
        if (u.use == UseDef::unused())
            u.use = ins;
        else if (u.use != ins)
            u.use = UseDef::multiuse();
    }

    bool used() const {
        assert(t == Type::Constant || t == Type::Value || t == Type::Any);
        return u.use != UseDef::unused();
    }

    struct UseDef {
        CodeEditor::Iterator def = unused();
        CodeEditor::Iterator use = unused();

        UseDef() {}
        UseDef(CodeEditor::Iterator def) : def(def) {}

        static CodeEditor::Iterator multiuse() {
            static CodeEditor::Iterator val(-2);
            return val;
        }

        static CodeEditor::Iterator unused() {
            static CodeEditor::Iterator val(-3);
            return val;
        }

        bool operator==(const UseDef& other) const {
            return def == other.def && use == other.use;
        }

        bool mergeWith(const UseDef& other) {
            bool changed = false;
            if (def != multiuse() && other.def != unused() &&
                def != other.def) {
                def = multiuse();
                changed = true;
            }
            if (use != multiuse() && other.use != unused() &&
                use != other.use) {
                use = multiuse();
                changed = true;
            }
            return changed;
        }
    };

    int argument = -1;
    UseDef u;

    bool isValue() const { return t == Type::Value || t == Type::Constant; }

    bool hasUseDef() const {
        return t == Type::Value || t == Type::Constant || t == Type::Any;
    }

    bool isPresent() const {
        return t == Type::Value || t == Type::Constant || t == Type::Argument ||
               t == Type::Any;
    }

    bool operator==(FValue const& other) const {
        if (t != other.t)
            return false;

        switch (t) {
        case Type::Constant:
        case Type::Value:
        case Type::Any:
            return u == other.u;
        case Type::Argument:
            return argument == other.argument;
        default:
            break;
        }
        return true;
    }

    bool operator!=(FValue const& other) const { return !(*this == other); }

    bool mergeWith(FValue const& other) {
        if ((other.t == Type::Absent) != (t == Type::Absent)) {
            t = Type::Maybe;
            return true;
        }

        bool changed = false;

        assert(!other.hasUseDef() || other.u.def != UseDef::unused());
        // Make sure we do not end up with an invalid use-def information
        // when we merge a state which has none
        if (hasUseDef() && !other.hasUseDef()) {
            u.def = UseDef::multiuse();
            u.use = UseDef::multiuse();
            changed = true;
        }

        switch (t) {
        case Type::Bottom:
            t = other.t;
            u = other.u;
            argument = other.argument;
            return true;
        case Type::Absent:
            assert(other.t == Type::Absent);
            return false;
        case Type::Maybe:
            return false;
        case Type::Argument:
            if (other.t != Type::Argument || argument != other.argument) {
                if (other.isPresent())
                    t = Type::Any;
                else
                    t = Type::Maybe;
                changed = true;
            }
            break;
        case Type::Constant:
            if (other.t != Type::Constant) {
                if (other.isValue())
                    t = Type::Value;
                else if (other.isPresent())
                    t = Type::Any;
                else
                    t = Type::Maybe;
                changed = true;
            } else if (u.def != other.u.def) {
                // Two defining sites, we cannot track the constant anymore
                t = Type::Value;
                changed = true;
            }
            break;
        case Type::Value:
            if (!other.isValue()) {
                if (other.isPresent())
                    t = Type::Any;
                else
                    t = Type::Maybe;
                changed = true;
            }
            break;
        case Type::Any:
            if (!other.isPresent()) {
                t = Type::Maybe;
                changed = true;
            }
            break;
        }

        changed = u.mergeWith(other.u) || changed;

        assert(t != Type::Constant || singleDef());
        assert(t != Type::Any || u.def != UseDef::unused());

        return changed;
    }

    static FValue const& bottom() {
        static FValue value;
        return value;
    }

    void print() const {
        switch (t) {
        case Type::Constant:
            std::cout << "const(" << singleDef() << used() << ")";
            break;
        case Type::Argument:
            std::cout << "arg " << argument;
            break;
        case Type::Bottom:
            std::cout << "??";
            break;
        case Type::Absent:
            std::cout << "absent";
            break;
        case Type::Value:
            std::cout << "Value";
            break;
        case Type::Maybe:
            std::cout << "Maybe";
            break;
        case Type::Any:
            std::cout << "Any";
            break;
        }
    }
};

enum class Type { Conservative, NoReflection, NoReflectionDelete };

template <Type type>
class DataflowAnalysis : public ForwardAnalysisIns<AbstractState<FValue>>,
                         public InstructionDispatcher::Receiver {

  public:
    DataflowAnalysis() : dispatcher_(*this) {}

    SEXP constant(FValue v) {
        assert(v.t == FValue::Type::Constant);
        assert(v.singleDef());
        BC bc = *v.u.def;
        if (bc.bc == BC_t::push_)
            return bc.immediateConst();
        if (bc.bc == BC_t::guard_fun_)
            return Pool::get(bc.immediate.guard_fun_args.expected);
        if (bc.bc == BC_t::ldvar_ || bc.bc == BC_t::ldddvar_ ||
            bc.bc == BC_t::ldfun_)
            return constant((*this)[v.u.def][(*v.u.def).immediateConst()]);
        if (bc.bc == BC_t::dup_ || bc.bc == BC_t::dup2_)
            return constant((*this)[v.u.def].top());

        assert(false);
        return R_NilValue;
    }

  protected:
    virtual Dispatcher& dispatcher() override { return dispatcher_; }

    AbstractState<FValue>* initialState() override {
        auto* result = new AbstractState<FValue>();
        int i = 0;
        for (SEXP x : code_->arguments()) {
            (*result)[x] = FValue::Argument(i++);
        }
        return result;
    }

    void swap_(CodeEditor::Iterator ins) override {
        auto a = current().pop();
        auto b = current().pop();
        a.used(ins);
        b.used(ins);
        current().push(a);
        current().push(b);
    }

    void pull_(CodeEditor::Iterator ins) override {
        int n = (*ins).immediate.i;
        auto& v = current().stack()[n];
        v.used(ins);
        current().push(v);
    }

    void put_(CodeEditor::Iterator ins) override {
        int n = (*ins).immediate.i;
        auto& v = current().top();
        v.used(ins);
        for (int i = 0; i < n - 1; i++)
            current()[i] = current()[i + i];
        current()[n] = v;
    }

    void ldfun_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];

        switch (v.t) {
        case FValue::Type::Constant:
            // If ldfun loads a known function there are no sideeffects,
            // otherwise we cannot be sure, since in the next scope there might
            // be an unevaluated promise with the same name.
            if (TYPEOF(constant(v)) == CLOSXP ||
                TYPEOF(constant(v)) == BUILTINSXP ||
                TYPEOF(constant(v)) == SPECIALSXP) {
                current().push(v.duplicate(ins));
            } else {
                doCall();
                current().push(FValue::Value(ins));
            }
            break;
        case FValue::Type::Argument:
        case FValue::Type::Any:
        case FValue::Type::Maybe:
            doCall();
            current().push(FValue::Value(ins));
            break;
        case FValue::Type::Value:
            current().push(v.duplicate(ins));
            break;
        case FValue::Type::Absent:
        case FValue::Type::Bottom:
            doCall();
            current().push(FValue::Value(ins));
            break;
        }
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];
        if (!v.isValue())
            doCall();
        if (v == FValue::bottom())
            current().push(FValue::Value(ins));
        else
            current().push(v.isValue() ? v.duplicate(ins) : FValue::Value(ins));
    }

    void ldddvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];
        if (!v.isValue())
            doCall();
        if (v == FValue::bottom())
            current().push(FValue::Value(ins));
        else
            current().push(v.isValue() ? v.duplicate(ins) : FValue::Value(ins));
    }

    void ldarg_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];
        if (!v.isValue())
            doCall();
        if (v == FValue::bottom())
            current().push(FValue::Value(ins));
        else
            current().push(v.isValue() ? v.duplicate(ins) : FValue::Value(ins));
    }

    void force_(CodeEditor::Iterator ins) override {
        auto v = current().pop();
        if (v.isValue()) {
            current().push(v);
        } else {
            doCall();
            current().push(FValue::Value(ins));
        }
    }

    void stvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current().pop();
        // <- is eager, this has to be a value
        if (!v.isValue())
            v = FValue::Value(FValue::UseDef::multiuse());
        current()[sym] = v;
    }

    void call_(CodeEditor::Iterator ins) override {
        current().pop();
        // TODO we could be fancy and if its known whether fun is eager or lazy
        // analyze the promises accordingly
        doCall();
        current().push(FValue::Any(ins));
    }

    void call_stack_(CodeEditor::Iterator ins) override {
        bool noProms = true;
        for (size_t i = 0; i < (*ins).immediate.call_args.nargs; ++i) {
            if (!current().pop().isValue())
                noProms = false;
        }
        if (!noProms)
            doCall();

        auto fun = current().pop();

        bool safeTarget = false;
        if (fun.t == FValue::Type::Constant) {
            if (TYPEOF(constant(fun)) == BUILTINSXP ||
                TYPEOF(constant(fun)) == SPECIALSXP) {
                int prim = constant(fun)->u.primsxp.offset;
                safeTarget = isSafeBuiltin(prim);
            }
        }
        if (!safeTarget) {
            doCall();
            current().push(FValue::Any(ins));
        } else {
            current().push(FValue::Value(ins));
        }
    }

    void static_call_stack_(CodeEditor::Iterator ins) override {
        CallSite cs = ins.callSite();
        SEXP fun = cs.target();

        bool noProms = true;
        for (size_t i = 0; i < (*ins).immediate.call_args.nargs; ++i) {
            if (!current().pop().isValue())
                noProms = false;
        }
        if (!noProms)
            doCall();

        bool safeTarget = false;
        if (TYPEOF(fun) == BUILTINSXP || TYPEOF(fun) == SPECIALSXP) {
            int prim = fun->u.primsxp.offset;
            safeTarget = isSafeBuiltin(prim);
        }
        if (!safeTarget) {
            doCall();
            current().push(FValue::Any(ins));
        } else {
            current().push(FValue::Value(ins));
        }
    }

    void dispatch_(CodeEditor::Iterator ins) override {
        // function
        current().pop();
        doCall();
        current().push(FValue::Any(ins));
    }

    void dispatch_stack_(CodeEditor::Iterator ins) override {
        // function
        current().pop((*ins).immediate.call_args.nargs);
        doCall();
        current().push(FValue::Any(ins));
    }

    void guard_fun_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        SEXP sym = Pool::get(bc.immediate.guard_fun_args.name);
        // TODO: is this not sound! There could be a higher up promise with the
        // functions name, which invalidates the local env, although very
        // unlikely. Thus we should instead fix guard_fun to work without
        // forcing promises.
        if (current()[sym].t == FValue::Type::Argument ||
            current()[sym].t == FValue::Type::Any)
            doCall();
        // TODO this is not quite right, since its most likely coming from a
        // parent environment
        current()[sym] = FValue::Constant(ins);
    }

    void pick_(CodeEditor::Iterator ins) override {
        int n = (*ins).immediate.i;
        auto& v = current()[n];
        v.used(ins);
        for (int i = 0; i < n - 1; i++)
            current()[i + 1] = current()[i];
        current().top() = v;
    }

    void label(CodeEditor::Iterator ins) override {}

    void doCall() {
        if (type == Type::NoReflectionDelete)
            current().mergeAllEnv(FValue::Any());
        else if (type == Type::Conservative)
            current().mergeAllEnv(FValue::Maybe());
    }

    void uniq_(CodeEditor::Iterator ins) override { current().top().used(ins); }

    void brobj_(CodeEditor::Iterator ins) override {
        current().top().used(ins);
    }

    void any(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        // pop as many as we need, push as many tops as we need
        current().pop(bc.popCount());
        if (!bc.isPure())
            doCall();

        switch (bc.bc) {
        // We know those BC do not return promises
        case BC_t::inc_:
        case BC_t::is_:
        case BC_t::isfun_:
        case BC_t::extract1_:
        case BC_t::subset1_:
        case BC_t::extract2_:
        case BC_t::subset2_:
        case BC_t::close_:
        case BC_t::lgl_or_:
        case BC_t::lgl_and_:
        case BC_t::test_bounds_:
        case BC_t::seq_:
        case BC_t::names_:
        case BC_t::length_:
        case BC_t::alloc_:
            for (size_t i = 0, e = bc.pushCount(); i != e; ++i)
                current().push(FValue::Value(ins));
            break;

        default:
            for (size_t i = 0, e = bc.pushCount(); i != e; ++i)
                current().push(FValue::Any(ins));
            break;
        }
    }

    void dup_(CodeEditor::Iterator ins) override {
        auto v = current().pop();
        v.used(ins);
        current().push(v.duplicate(ins));
        current().push(v);
    }

    void dup2_(CodeEditor::Iterator ins) override {
        auto& a = current().stack()[0];
        auto& b = current().stack()[1];
        a.used(ins);
        b.used(ins);
        current().push(a.duplicate(ins));
        current().push(b.duplicate(ins));
    }

    void return_(CodeEditor::Iterator ins) override {
        // Return is also a leave instruction
        current().pop(current().stack().depth());
    }

    void alloc_(CodeEditor::Iterator ins) override {
        current().push(FValue::Value(ins));
    }

    void push_(CodeEditor::Iterator ins) override {
        current().push(FValue::Constant(ins));
    }

    InstructionDispatcher dispatcher_;
};
}
#endif
