#ifndef RIR_OPTIMIZER_SCOPE_H
#define RIR_OPTIMIZER_SCOPE_H

#include "ir/CodeEditor.h"
#include "R/Funtab.h"
#include "code/analysis.h"
#include "code/dispatchers.h"
#include "interpreter/interp_context.h"

#include <unordered_set>

namespace rir {

/*
 *                                  Any
 *
 *                Value
 *
 *          Constant  LocalVar                  Argument
 */

class PointsTo {
  public:
    enum class Type { Bottom, Constant, Argument, LocalVar, Value, Any };

    Type t = Type::Bottom;

    PointsTo() {}
    PointsTo(Type t) : t(t) {
        assert(t == Type::Any || t == Type::Bottom || t == Type::Value);
    }
    PointsTo(const PointsTo& p) : t(p.t), value(p.value) {}

    static PointsTo Argument(int i) {
        PointsTo p;
        p.t = Type::Argument;
        p.value.argument = i;
        return p;
    }

    static PointsTo Constant(CodeEditor::Iterator def) {
        PointsTo p;
        p.t = Type::Constant;
        p.value.def = def;
        return p;
    }

    static PointsTo LocalVar(CodeEditor::Iterator def) {
        PointsTo p;
        p.t = Type::LocalVar;
        p.value.def = def;
        return p;
    }

    static PointsTo Any() { return PointsTo(Type::Any); }

    static PointsTo Value() { return PointsTo(Type::Value); }

    PointsTo load(AbstractState<PointsTo>& state) {
        if (t == Type::LocalVar) {
            assert((*value.def).bc == BC_t::stvar_);
            SEXP name = (*value.def).immediateConst();
            return state[name];
        }
        assert(false);
        return PointsTo();
    }

    union AValue {
        int argument;
        CodeEditor::Iterator def;
        AValue() {}
        AValue(const AValue& v) { *this = v; }
    };
    AValue value;

    bool isValue() {
        return t == Type::Value || t == Type::Constant || t == Type::LocalVar;
    }

    SEXP constant() {
        assert(t == Type::Constant);
        BC bc = *value.def;
        if (bc.bc == BC_t::push_)
            return bc.immediateConst();
        if (bc.bc == BC_t::guard_fun_)
            return Pool::get(bc.immediate.guard_fun_args.expected);
        assert(false);
        return R_NilValue;
    }

    bool pure() const {
        // Type::Value is already evaluated!
        return t != Type::Any && t != Type::Argument;
    }

    bool operator==(PointsTo const& other) const {
        if (t != other.t)
            return false;

        switch (t) {
        case Type::Constant:
        case Type::LocalVar:
            return value.def == other.value.def;
        case Type::Argument:
            return value.argument == other.value.argument;
        default:
            break;
        }
        return true;
    }

    bool operator!=(PointsTo const& other) const { return !(*this == other); }

    bool mergeWith(PointsTo const& other) {
        if (other.t == Type::Bottom)
            return false;

        switch (t) {
        case Type::Bottom:
            t = other.t;
            value = other.value;
            return t != Type::Bottom;
        case Type::Argument:
            if (other.t != Type::Argument ||
                value.argument != other.value.argument) {
                t = Type::Any;
                return true;
            }
            return false;
        case Type::Constant:
            if (other.t != Type::Constant) {
                t = other.pure() ? Type::Value : Type::Any;
                return true;
            }
            if (value.def != other.value.def) {
                t = Type::Value;
                return true;
            }
            return false;
        case Type::LocalVar:
            if (other.t != Type::LocalVar) {
                t = other.pure() ? Type::Value : Type::Any;
                return true;
            }
            if (value.def != other.value.def) {
                t = Type::Value;
                return true;
            }
            return false;
        case Type::Value:
            if (!other.pure()) {
                t = Type::Any;
                return true;
            }
            return false;
        case Type::Any:
            return false;
        }
        assert(false && "unreachable");
        return false;
    }

    static PointsTo const& top() {
        static PointsTo value(Type::Any);
        return value;
    }

    static PointsTo const& bottom() {
        static PointsTo value(Type::Bottom);
        return value;
    }

    void print() const {
        switch (t) {
        case Type::Constant:
            std::cout << "const from ";
            (*value.def).print();
            break;
        case Type::Argument:
            std::cout << "arg " << value.argument;
            break;
        case Type::LocalVar:
            std::cout << "defined at ";
            (*value.def).print();
            break;
        case Type::Bottom:
            std::cout << "??";
            break;
        case Type::Value:
            std::cout << "Value";
            break;
        case Type::Any:
            std::cout << "Any";
            break;
        }
    }
};

enum class Type { Conservative, NoReflection };

template <Type type>
class ScopeResolution : public ForwardAnalysisIns<AbstractState<PointsTo>>,
                        public InstructionDispatcher::Receiver {

  public:
    ScopeResolution() : dispatcher_(*this) {}

  protected:
    virtual Dispatcher& dispatcher() override { return dispatcher_; }

    AbstractState<PointsTo>* initialState() override {
        auto* result = new AbstractState<PointsTo>();
        int i = 0;
        for (SEXP x : code_->arguments()) {
            (*result)[x] = PointsTo::Argument(i++);
        }
        return result;
    }

    void swap_(CodeEditor::Iterator ins) override {
        auto a = current().pop();
        auto b = current().pop();
        current().push(a);
        current().push(b);
    }

    void pull_(CodeEditor::Iterator ins) override {
        int n = (*ins).immediate.i;
        current().push(current()[n]);
    }

    void put_(CodeEditor::Iterator ins) override {
        int n = (*ins).immediate.i;
        auto v = current().top();
        for (int i = 0; i < n - 1; i++)
            current()[i] = current()[i + i];
        current()[n] = v;
    }

    void ldfun_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];

        switch (v.t) {
        case PointsTo::Type::Constant:
            // If ldfun loads a known function there are no sideeffects,
            // otherwise we cannot be sure, since in the next scope there might
            // be an unevaluated promise with the same name.
            if (TYPEOF(v.constant()) == CLOSXP ||
                TYPEOF(v.constant()) == BUILTINSXP ||
                TYPEOF(v.constant()) == SPECIALSXP) {
                current().push(v);
            } else {
                doCall();
                current().push(PointsTo::Value());
            }
            break;
        case PointsTo::Type::Argument:
        case PointsTo::Type::Any:
            doCall();
            current().push(PointsTo::Value());
            break;
        case PointsTo::Type::LocalVar:
        case PointsTo::Type::Value:
            current().push(v);
            break;
        case PointsTo::Type::Bottom:
            doCall();
            current().push(PointsTo::Value());
            break;
        }
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];
        if (!v.pure())
            doCall();
        if (v == PointsTo::bottom())
            current().push(PointsTo::Value());
        else
            current().push(v.pure() ? v : PointsTo::Value());
    }

    void call_(CodeEditor::Iterator ins) override {
        current().pop();
        // TODO we could be fancy and if its known whether fun is eager or lazy
        // analyze the promises accordingly
        doCall();
        current().push(PointsTo::Any());
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
        if (fun.t == PointsTo::Type::Constant) {
            if (TYPEOF(fun.constant()) == BUILTINSXP ||
                TYPEOF(fun.constant()) == SPECIALSXP) {
                int prim = fun.constant()->u.primsxp.offset;
                safeTarget = isSafeBuiltin(prim);
            }
        }
        if (!safeTarget) {
            doCall();
            current().push(PointsTo::Any());
        } else {
            current().push(PointsTo::Value());
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
            current().push(PointsTo::Any());
        } else {
            current().push(PointsTo::Value());
        }
    }

    void dispatch_(CodeEditor::Iterator ins) override {
        // function
        current().pop();
        doCall();
        current().push(PointsTo::Any());
    }

    void dispatch_stack_(CodeEditor::Iterator ins) override {
        // function
        current().pop((*ins).immediate.call_args.nargs);
        doCall();
        current().push(PointsTo::Any());
    }

    void guard_fun_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        SEXP sym = Pool::get(bc.immediate.guard_fun_args.name);
        // TODO this is not quite right, since its most likely coming from a
        // parent environment
        current()[sym] = PointsTo::Constant(ins);
    }

    void pick_(CodeEditor::Iterator ins) override {
        int n = (*ins).immediate.i;
        auto v = current()[n];
        for (int i = 0; i < n - 1; i++)
            current()[i + 1] = current()[i];
        current().top() = v;
    }

    void label(CodeEditor::Iterator ins) override {}

    void doCall() {
        if (type == Type::Conservative)
            current().mergeAllEnv(PointsTo::Type::Any);
    }

    void any(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        // pop as many as we need, push as many tops as we need
        current().pop(bc.popCount());
        if (!bc.isPure())
            doCall();
        for (size_t i = 0, e = bc.pushCount(); i != e; ++i)
            current().push(PointsTo::Type::Any);
    }

    void ldddvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];
        if (v.pure()) {
            current().push(v);
        } else {
            doCall();
            current().push(PointsTo::Value());
        }
    }

    void ldarg_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];
        if (v.pure()) {
            current().push(v);
        } else {
            doCall();
            current().push(PointsTo::Value());
        }
    }

    void force_(CodeEditor::Iterator ins) override {
        auto v = current().pop();
        if (v.pure()) {
            current().push(v);
        } else {
            doCall();
            current().push(PointsTo::Value());
        }
    }

    void stvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current().pop();
        if (!v.pure())
            v = PointsTo::Value();
        current()[sym] = v;
    }

    void return_(CodeEditor::Iterator ins) override {
        // Return is also a leave instruction
        current().pop(current().stack().depth());
    }

    void alloc_(CodeEditor::Iterator ins) override {
        current().push(PointsTo::Type::Value);
    }

    void push_(CodeEditor::Iterator ins) override {
        current().push(PointsTo::Constant(ins));
    }

    InstructionDispatcher dispatcher_;
};
}
#endif
