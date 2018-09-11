#include "../pir/pir_impl.h"
#include "query.h"
#include "scope.h"

#include <algorithm>

namespace rir {
namespace pir {

AbstractPirValue::AbstractPirValue() : type(PirType::bottom()) {}
AbstractPirValue::AbstractPirValue(Value* v, Instruction* o) : type(v->type) {
    vals.insert(ValOrig(v, o));
}

bool AbstractPirValue::merge(const AbstractPirValue& other) {
    assert(other.type != PirType::bottom());

    if (unknown)
        return false;
    if (type == PirType::bottom()) {
        *this = other;
        return true;
    }
    if (other.unknown) {
        unknown = true;
        return true;
    }

    bool changed = false;
    if (!std::includes(vals.begin(), vals.end(), other.vals.begin(),
                       other.vals.end())) {
        vals.insert(other.vals.begin(), other.vals.end());
        changed = true;
    }

    return changed;
}

void AbstractPirValue::print(std::ostream& out) {
    if (unknown) {
        out << "??";
        return;
    }
    out << "(";
    for (auto it = vals.begin(); it != vals.end();) {
        auto vo = *it;
        vo.val->printRef(out);
        out << "@";
        vo.origin->printRef(out);
        it++;
        if (it != vals.end())
            out << "|";
    }
    out << ") : " << type;
}

MkFunCls* AbstractREnvironmentHierarchy::findClosure(Value* env, Value* fun) {
    for (;;) {
        if (Force::Cast(fun)) {
            fun = Force::Cast(fun)->input();
        } else if (ChkClosure::Cast(fun)) {
            fun = ChkClosure::Cast(fun)->arg<0>().val();
        } else {
            break;
        }
    }
    while (env && env != AbstractREnvironment::UnknownParent) {
        if ((*this)[env].mkClosures.count(fun))
            return (*this)[env].mkClosures.at(fun);
        env = (*this)[env].parentEnv();
    }
    return AbstractREnvironment::UnknownClosure;
}

std::unordered_set<Value*>
AbstractREnvironmentHierarchy::potentialParents(Value* env) const {
    std::unordered_set<Value*> res;
    assert(env);
    while (this->count(env)) {
        res.insert(env);
        auto aenv = this->at(env);
        auto parent = at(env).parentEnv();
        assert(parent);
        if (parent == AbstractREnvironment::UnknownParent &&
            Env::parentEnv(env))
            env = Env::parentEnv(env);
        else
            env = parent;
        if (env == Env::nil())
            return res;
    }
    // We did not reach the outer most environment of the current closure.
    // Therefore we have no clue which envs are the actual parents. The
    // conservative choice is to return all candidates.
    for (auto e : *this)
        res.insert(e.first);
    return res;
}

AbstractLoad AbstractREnvironmentHierarchy::get(Value* env, SEXP e) const {
    assert(env);
    while (env != AbstractREnvironment::UnknownParent) {
        if (this->count(env) == 0)
            return AbstractLoad(env ? env : AbstractREnvironment::UnknownParent,
                                AbstractPirValue::tainted());
        auto aenv = this->at(env);
        if (!aenv.absent(e)) {
            const AbstractPirValue& res = aenv.get(e);
            return AbstractLoad(env, res);
        }
        auto parent = at(env).parentEnv();
        assert(parent);
        if (parent == AbstractREnvironment::UnknownParent &&
            Env::parentEnv(env))
            env = Env::parentEnv(env);
        else
            env = parent;
    }
    return AbstractLoad(env, AbstractPirValue::tainted());
}

AbstractLoad AbstractREnvironmentHierarchy::superGet(Value* env, SEXP e) const {
    if (!count(env))
        return AbstractLoad(AbstractREnvironment::UnknownParent,
                            AbstractPirValue::tainted());
    auto parent = at(env).parentEnv();
    assert(parent);
    if (parent == AbstractREnvironment::UnknownParent && Env::parentEnv(env))
        parent = Env::parentEnv(env);
    return get(parent, e);
}

Value* AbstractREnvironment::UnknownParent = (Value*)-1;
Value* AbstractREnvironment::UninitializedParent = (Value*)-2;
MkFunCls* AbstractREnvironment::UnknownClosure = (MkFunCls*)-1;
}
}
