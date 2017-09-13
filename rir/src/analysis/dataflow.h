#ifndef RIR_ANALYSIS_DATAFLOW_H
#define RIR_ANALYSIS_DATAFLOW_H

#include "R/Funtab.h"
#include "R/r.h"
#include "analysis_framework/analysis.h"
#include "analysis_framework/dispatchers.h"
#include "interpreter/interp_context.h"
#include "ir/CodeEditor.h"

namespace rir {

/*
 *                       Maybe  (might be deleted from env)
 *                    /        \
 *              Absent          Any (prom, R_MissingArg)
 *                          /         \
 *                      Value         Argument
 *                        |
 *                    Constant
 *
 *                  \     |     /
 *                      Bottom
 */

class FValue {
  public:
    enum class Type { Bottom, Absent, Constant, Argument, Value, Any, Maybe };

  private:
    FValue(Type t, SEXP argument) : t(t), argument(argument) {}

    FValue(Type t, CodeEditor::Iterator def = UseDef::unused())
        : t(t), u(def) {}

  public:
    Type t = Type::Bottom;

    FValue() {}
    FValue(const FValue& p) : t(p.t), argument(p.argument), u(p.u) {}

    static FValue Argument(SEXP a = nullptr) {
        return FValue(Type::Argument, a);
    }
    static FValue Constant(CodeEditor::Iterator def) {
        return FValue(Type::Constant, def);
    }
    static FValue Maybe() { return FValue(Type::Maybe); }
    static FValue Any(CodeEditor::Iterator ins = UseDef::multiuse()) {
        return FValue(Type::Any, ins);
    }
    static FValue Value(CodeEditor::Iterator ins = UseDef::multiuse()) {
        return FValue(Type::Value, ins);
    }
    static FValue Absent() { return FValue(Type::Absent); }
    static FValue Bottom() { return FValue(Type::Bottom); }
    static const FValue& bottom() {
        static FValue val = FValue(Type::Bottom);
        return val;
    }
    static const FValue& top() {
        static FValue val = FValue(Type::Maybe);
        return val;
    }

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

    bool singleUse() const {
        if (hasUseDef()) {
            assert(u.use != UseDef::unused());
            return u.use != UseDef::multiuse();
        }
        return false;
    }

    CodeEditor::Iterator use() {
        assert(singleUse());
        return u.use;
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

    SEXP argument = nullptr;
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
#ifdef ENABLE_SLOWASSERT
        FValue old = *this;
        bool res = doMergeWith(other);

        if (old.t != t) {
            switch (old.t) {
            case Type::Absent:
                assert(t == Type::Maybe);
                break;
            case Type::Constant:
                assert(t == Type::Value || t == Type::Any || t == Type::Maybe);
                break;
            case Type::Value:
                assert(t == Type::Any || t == Type::Maybe);
                break;
            case Type::Any:
                assert(t == Type::Maybe);
                break;
            case Type::Maybe:
                assert(false);
                break;
            case Type::Argument:
                assert(t == Type::Any || t == Type::Maybe);
                break;
            case Type::Bottom:
                break;
            }
        }
        if (res) {
            assert(old.t != t || old.u.def != u.def || old.u.use != u.use);
        }
        return res;
    }

    bool doMergeWith(FValue const& other) {
#endif
        bool changed = false;

        if ((other.t == Type::Absent) != (t == Type::Absent)) {
            changed = t != Type::Maybe;
            t = Type::Maybe;
            return changed;
        }

        assert(!other.hasUseDef() || other.u.def != UseDef::unused());
        // Make sure we do not end up with an invalid use-def information
        // when we merge a state which has none
        if (hasUseDef() && !other.hasUseDef()) {
            if (u.def != UseDef::multiuse()) {
                u.def = UseDef::multiuse();
                changed = true;
            }
            if (u.use != UseDef::multiuse()) {
                u.use = UseDef::multiuse();
                changed = true;
            }
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
            // Both are argument, but arg names are not the same ->
            // check if one of them is bottom
            if (other.t == Type::Argument && argument != other.argument) {
                if (!argument) {
                    argument = other.argument;
                    changed = true;
                    break;
                }
                if (!other.argument)
                    break;
            }
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

    void print() const {
        switch (t) {
        case Type::Constant:
            Rprintf("const(%d%d)", singleDef(), used());
            break;
        case Type::Argument:
            Rprintf("arg %d", argument);
            break;
        case Type::Bottom:
            Rprintf("??");
            break;
        case Type::Absent:
            Rprintf("absent");
            break;
        case Type::Value:
            Rprintf("Value");
            break;
        case Type::Maybe:
            Rprintf("Maybe");
            break;
        case Type::Any:
            Rprintf("Any");
            break;
        }
    }
};

enum class Type {
    Conservative,
    NoDelete,
    NoDeleteNoPromiseStore,
    NoArgsOverride,
    Optimistic
};

// Executing promises can only affect the local environment if we
// execute a default argument (since this one has the local
// environment) or if we leak our environment through another channel
class FGlobal {
  public:
    bool leaksEnvironment = false;
    bool mergeWith(const FGlobal* other) {
        if (!leaksEnvironment && other->leaksEnvironment) {
            leaksEnvironment = true;
            return true;
        }
        return false;
    }
};

template <Type type>
class DataflowAnalysis
    : public ForwardAnalysisIns<AbstractState<SEXP, FValue, FGlobal>>,
      public InstructionDispatcher::Receiver {

  public:
    DataflowAnalysis() : dispatcher_(*this) {}

    SEXP constant(FValue v) {
        assert(v.t == FValue::Type::Constant);
        assert(v.singleDef());
        BC bc = *v.u.def;
        if (bc.bc == Opcode::push_)
            return bc.immediateConst();
        if (bc.bc == Opcode::guard_fun_)
            return Pool::get(bc.immediate.guard_fun_args.expected);
        if (bc.bc == Opcode::ldvar_ || bc.bc == Opcode::ldddvar_ ||
            bc.bc == Opcode::ldfun_ || bc.bc == Opcode::ldlval_ ||
            bc.bc == Opcode::ldarg_)
            return constant((*this)[v.u.def][(*v.u.def).immediateConst()]);
        if (bc.bc == Opcode::dup_ || bc.bc == Opcode::dup2_)
            return constant((*this)[v.u.def].top());

        assert(false);
        return R_NilValue;
    }

  protected:
    virtual Dispatcher& dispatcher() override { return dispatcher_; }

    AbstractState<SEXP, FValue, FGlobal>* initialState() override {
        auto* result = new AbstractState<SEXP, FValue, FGlobal>();
        for (auto a : code_->arguments()) {
            (*result)[a.first] = FValue::Argument(a.first);
            if (a.second != R_MissingArg &&
                (TYPEOF(a.second) == LANGSXP || TYPEOF(a.second) == SYMSXP)) {
                result->global().leaksEnvironment = true;
            }
        }

        return result;
    }

    void guard_env_(CodeEditor::Iterator ins) override {
        // Make sure that the stack remains unchanged across guards
        for (auto v : current().stack())
            v.used(ins);
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
                if (current().global().leaksEnvironment)
                    doCall(ins);
                current().push(FValue::Value(ins));
            }
            break;
        case FValue::Type::Argument:
        case FValue::Type::Any:
        case FValue::Type::Maybe:
            if (current().global().leaksEnvironment)
                doCall(ins);
            current().push(FValue::Value(ins));
            break;
        case FValue::Type::Value:
            current().push(v.duplicate(ins));
            break;
        case FValue::Type::Absent:
        case FValue::Type::Bottom:
            if (current().global().leaksEnvironment)
                doCall(ins);
            current().push(FValue::Value(ins));
            break;
        }
    }

    FValue forceProm(FValue val, CodeEditor::Iterator ins) {
        // If the local environment might have been leaked and we don't know if
        // that the promise was forced already, we have to assume the worst
        // (ie. its as bad as calling random code).
        if (!val.isValue() && current().global().leaksEnvironment)
            doCall(ins);

        // If the promise is forced already we might know more
        if (val.isValue())
            return val.duplicate(ins);

        // The result of forcing a promise is NOT necessarily a value. It might
        // be R_MissingArg
        return FValue::Any(ins);
    }

    void ldlval_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];
        if (v.isValue())
            v = v.duplicate(ins);
        else
            v = FValue::Value(ins);
        current()[sym] = v;
        current().push(v);
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = forceProm(current()[sym], ins);
        if (current()[sym].isPresent())
            current()[sym] = v;
        current().push(v);
    }

    void ldddvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = forceProm(current()[sym], ins);
        if (current()[sym].isPresent())
            current()[sym] = v;
        current().push(v);
    }

    void ldarg_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = forceProm(current()[sym], ins);
        if (current()[sym].isPresent())
            current()[sym] = v;
        current().push(v);
    }

    void force_(CodeEditor::Iterator ins) override {
        current().push(forceProm(current().pop(), ins));
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
        doCall(ins);
        current().push(FValue::Any(ins));
    }

    void call_stack_(CodeEditor::Iterator ins) override {
        bool noProms = true;
        for (size_t i = 0; i < (*ins).immediate.call_args.nargs; ++i) {
            if (!current().pop().isValue())
                noProms = false;
        }
        if (!noProms)
            doCall(ins);

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
            doCall(ins);
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
            doCall(ins);

        bool safeTarget = false;
        if (TYPEOF(fun) == BUILTINSXP || TYPEOF(fun) == SPECIALSXP) {
            int prim = fun->u.primsxp.offset;
            safeTarget = isSafeBuiltin(prim);
        }
        if (!safeTarget) {
            doCall(ins);
            current().push(FValue::Any(ins));
        } else {
            current().push(FValue::Value(ins));
        }
    }

    void dispatch_(CodeEditor::Iterator ins) override {
        // function
        current().pop();
        doCall(ins);
        current().push(FValue::Any(ins));
    }

    void dispatch_stack_(CodeEditor::Iterator ins) override {
        // function
        current().pop((*ins).immediate.call_args.nargs);
        doCall(ins);
        current().push(FValue::Any(ins));
    }

    void guard_fun_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        SEXP sym = Pool::get(bc.immediate.guard_fun_args.name);
        // TODO: is this not sound! There coud be a higher up promise with the
        // functions name, which invalidates the local env, although very
        // unlikely. Thus we should instead fix guard_fun to work without
        // forcing promises.
        if (current().global().leaksEnvironment &&
            (current()[sym].t == FValue::Type::Argument ||
             current()[sym].t == FValue::Type::Any))
            doCall(ins);
        // TODO this is not quite right, since its most likely coming from a
        // parent environment
        // current()[sym] = FValue::Constant(ins);
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

    // Deal with the impact on the local environment when calling random code
    void doCall(CodeEditor::Iterator ins) {
        // If the call is proceeded by a guard, we can assume that:
        // 1. No binding gets deleted
        // 2. Arguments are not overwritten
        // 3. The environment is not leaked
        if ((ins + 1) != code_->end() && (*(ins + 1)).is(Opcode::guard_env_)) {
            for (auto& e : current().env())
                if (e.second.t != FValue::Type::Argument)
                    e.second.mergeWith(FValue::Value());
            return;
        }

        // Depending on the type of analysis, we apply different strategies.
        // Only Type::Conservative is sound!
        if (type == Type::NoDelete)
            current().mergeAllEnv(FValue::Any());
        else if (type == Type::NoDeleteNoPromiseStore)
            current().mergeAllEnv(FValue::Value());
        else if (type == Type::Conservative) {
            current().global().leaksEnvironment = true;
            current().mergeAllEnv(FValue::Maybe());
        } else if (type == Type::NoArgsOverride)
            current().mergeAllEnv(FValue::Argument());
    }

    void make_unique_(CodeEditor::Iterator ins) override {
        current().top().used(ins);
    }
    void set_shared_(CodeEditor::Iterator ins) override {
        current().top().used(ins);
    }

    void brobj_(CodeEditor::Iterator ins) override {
        current().top().used(ins);
    }

    void any(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        // pop as many as we need, push as many tops as we need
        current().pop(bc.popCount());
        if (!bc.isPure())
            doCall(ins);

        switch (bc.bc) {
        // We know those BC do not return promises
        case Opcode::inc_:
        case Opcode::is_:
        case Opcode::isfun_:
        case Opcode::extract1_:
        case Opcode::subset1_:
        case Opcode::extract2_:
        case Opcode::subset2_:
        case Opcode::close_:
        case Opcode::lgl_or_:
        case Opcode::lgl_and_:
        case Opcode::test_bounds_:
        case Opcode::seq_:
        case Opcode::names_:
        case Opcode::length_:
        case Opcode::alloc_:
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
