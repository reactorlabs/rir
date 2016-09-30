#ifndef RIR_OPTIMIZER_SCOPE_H
#define RIR_OPTIMIZER_SCOPE_H

#include "ir/CodeEditor.h"
#include "code/analysis.h"
#include "code/dispatchers.h"
#include "interpreter/interp_context.h"

#include <unordered_set>

namespace rir {

class PointsTo {
  public:
    enum class Type { Bottom, Constant, AnyValue, DefaultValue, Argument, Any };
    Type t;
    SEXP c;

    PointsTo(Type t) : t(t) {}
    PointsTo(Type t, SEXP c) : t(t), c(c) {}

    bool operator==(PointsTo const& other) const {
        if (t != other.t)
            return false;

        switch (t) {
        case Type::Constant:
        case Type::Argument:
        case Type::DefaultValue:
            return c == other.c;
        case Type::Bottom:
        case Type::AnyValue:
        case Type::Any:
            return true;
        }
        assert(false && "unreachable");
        return false;
    }

    bool operator!=(PointsTo const& other) const {
        if (t != other.t)
            return true;

        switch (t) {
        case Type::Constant:
        case Type::Argument:
        case Type::DefaultValue:
            return c != other.c;
        case Type::Bottom:
        case Type::AnyValue:
        case Type::Any:
            return false;
        }
        assert(false && "unreachable");
        return false;
    }

    bool mergeWith(PointsTo const& other) {
        switch (t) {
        case Type::Bottom:
            t = other.t;
            c = other.c;
            return t != Type::Bottom;
        case Type::Argument:
        case Type::DefaultValue:
            if (t != other.t || c != other.c) {
                t = Type::Any;
                return true;
            }
            return false;
        case Type::Constant:
            if (t != other.t) {
                t = Type::Any;
                return true;
            }
            if (c != other.c) {
                t = Type::AnyValue;
                return true;
            }
            return false;
        case Type::AnyValue:
            if (t != other.t) {
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
        static PointsTo value(Type::Bottom);
        return value;
    }

    static PointsTo const& bottom() {
        static PointsTo value(Type::Bottom);
        return value;
    }

    void print() const {
        switch (t) {
        case Type::Constant:
            Rf_PrintValue(c);
            break;
        case Type::Argument:
            std::cout << "arg: ";
            Rf_PrintValue(c);
            break;
        case Type::DefaultValue:
            std::cout << "guarded ";
            Rf_PrintValue(c);
            break;
        case Type::Bottom:
            std::cout << "??\n";
            break;
        case Type::AnyValue:
            std::cout << "Value\n";
            break;
        case Type::Any:
            std::cout << "Any\n";
            break;
        }
    }
};

class ScopeResolution : public ForwardAnalysisIns<AbstractState<PointsTo>>,
                        public InstructionDispatcher::Receiver {

  public:
    ScopeResolution() : dispatcher_(*this) {}

  protected:
    virtual Dispatcher& dispatcher() override { return dispatcher_; }

    AbstractState<PointsTo>* initialState() override {
        auto* result = new AbstractState<PointsTo>();
        for (SEXP x : code_->arguments()) {
            (*result)[x] = PointsTo(PointsTo::Type::Argument, x);
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
        case PointsTo::Type::DefaultValue:
        case PointsTo::Type::AnyValue:
            current().push(v);
            break;
        case PointsTo::Type::Constant:
            if (TYPEOF(v.c) == CLOSXP || TYPEOF(v.c) == BUILTINSXP ||
                TYPEOF(v.c) == SPECIALSXP) {
                current().push(v);
            } else {
                current().push(PointsTo::Type::Any);
                doCall();
            }
            break;
        case PointsTo::Type::Argument:
        case PointsTo::Type::Any:
            current().push(v);
            doCall();
            break;
        case PointsTo::Type::Bottom:
            current().push(PointsTo::Type::Any);
            doCall();
            break;
        }
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];
        switch (v.t) {
        case PointsTo::Type::Constant:
        case PointsTo::Type::DefaultValue:
        case PointsTo::Type::AnyValue:
            current().push(v);
            break;
        case PointsTo::Type::Argument:
        case PointsTo::Type::Any:
            current().push(v);
            doCall();
            break;
        case PointsTo::Type::Bottom:
            current().push(PointsTo::Type::Any);
            doCall();
            break;
        }
    }

    void call_(CodeEditor::Iterator ins) override {
        // function
        current().pop();
        current().push(PointsTo::Type::AnyValue);
        doCall();
    }

    void call_stack_(CodeEditor::Iterator ins) override {
        auto fun = current().stack()[(*ins).immediate.call_stack_args.nargs];
        current().pop((*ins).immediate.call_stack_args.nargs + 1);
        current().push(PointsTo::Type::AnyValue);

        if (fun.t == PointsTo::Type::Constant) {
            if (TYPEOF(fun.c) == BUILTINSXP) {
                std::string name(R_FunTab[fun.c->u.primsxp.offset].name);
                // We have reason to believe that those would not run arbitrary
                // code and not mess with the env
                if (name.compare("length") == 0 || name.compare("c") == 0)
                    return;
            }
        }

        doCall();
    }

    void dispatch_(CodeEditor::Iterator ins) override {
        // function
        current().pop();
        current().push(PointsTo::Type::AnyValue);
        doCall();
    }

    void dispatch_stack_(CodeEditor::Iterator ins) override {
        // function
        current().pop((*ins).immediate.dispatch_stack_args.nargs);
        current().push(PointsTo::Type::AnyValue);
        doCall();
    }

    void isspecial_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        current()[sym] = PointsTo(PointsTo::Type::DefaultValue, sym);
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
        current().mergeAllEnv(PointsTo::Type::Any);
    }

    void any(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        // pop as many as we need, push as many tops as we need
        current().pop(bc.popCount());
        for (size_t i = 0, e = bc.pushCount(); i != e; ++i)
            current().push(PointsTo::Type::Any);
    }

    void ldddvar_(CodeEditor::Iterator ins) override {
        current().push(PointsTo::Type::Any);
        doCall();
    }

    void ldarg_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = current()[sym];
        current().push(v);
        doCall();
    }

    void force_(CodeEditor::Iterator ins) override { doCall(); }

    void stvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        current()[sym] = current().pop();
    }

    void asbool_(CodeEditor::Iterator ins) override {
        current().pop();
        current().push(PointsTo::Type::AnyValue);
        doCall();
    }

    void mul_(CodeEditor::Iterator ins) override {
        current().pop();
        current().pop();
        current().push(PointsTo::Type::AnyValue);
        doCall();
    }

    void add_(CodeEditor::Iterator ins) override {
        current().pop();
        current().pop();
        current().push(PointsTo::Type::AnyValue);
        doCall();
    }

    void sub_(CodeEditor::Iterator ins) override {
        current().pop();
        current().pop();
        current().push(PointsTo::Type::AnyValue);
        doCall();
    }

    void lt_(CodeEditor::Iterator ins) override {
        current().pop();
        current().pop();
        current().push(PointsTo::Type::AnyValue);
        doCall();
    }

    void extract1_(CodeEditor::Iterator ins) override {
        current().pop();
        current().pop();
        current().push(PointsTo::Type::AnyValue);
    }

    void subset1_(CodeEditor::Iterator ins) override {
        current().pop();
        current().pop();
        current().push(PointsTo::Type::AnyValue);
    }

    void return_(CodeEditor::Iterator ins) override {
        // Return is also a leave instruction
        current().pop(current().stack().depth());
    }

    void aslogical_(CodeEditor::Iterator ins) override {
        current().pop();
        current().push(PointsTo::Type::AnyValue);
        doCall();
    }

    void subassign_(CodeEditor::Iterator ins) override {
        current().pop();
        current().pop();
        current().pop();
        current().push(PointsTo::Type::AnyValue);
    }

    void subassign2_(CodeEditor::Iterator ins) override {
        current().pop();
        current().pop();
        current().pop();
        current().push(PointsTo::Type::AnyValue);
    }

    void alloc_(CodeEditor::Iterator ins) override {
        current().push(PointsTo::Type::AnyValue);
    }

    void push_(CodeEditor::Iterator ins) override {
        SEXP val = Pool::get((*ins).immediate.pool);
        current().push(PointsTo(PointsTo::Type::Constant, val));
    }

    InstructionDispatcher dispatcher_;
};
}
#endif
