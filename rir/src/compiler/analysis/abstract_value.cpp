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

void AbstractREnvironmentHierarchy::print(std::ostream& out, bool tty) {
    for (auto& e : envs) {
        out << "== [";
        e.first->printRef(out);
        out << "]\n";
        e.second.print(out);
    }
    for (auto& a : aliases) {
        out << "* ";
        a.first->printRef(out);
        out << " = ";
        a.second->printRef(out);
        out << "\n";
    }
}

void AbstractREnvironment::print(std::ostream& out, bool tty) {
    if (leaked)
        out << "* leaked\n";
    if (tainted)
        out << "* tainted\n";

    for (auto e : entries) {
        SEXP name = std::get<0>(e);
        out << "   " << CHAR(PRINTNAME(name)) << " -> ";
        AbstractPirValue v = std::get<1>(e);
        v.print(out);
        out << "\n";
    }
}

AbstractResult AbstractPirValue::merge(const AbstractPirValue& other) {
    assert(other.type != PirType::bottom());

    if (unknown)
        return AbstractResult::None;
    if (type == PirType::bottom()) {
        *this = other;
        return AbstractResult::Updated;
    }
    if (other.unknown) {
        unknown = true;
        return AbstractResult::LostPrecision;
    }

    bool changed = false;
    if (!std::includes(vals.begin(), vals.end(), other.vals.begin(),
                       other.vals.end())) {
        vals.insert(other.vals.begin(), other.vals.end());
        changed = true;
    }

    return changed ? AbstractResult::Updated : AbstractResult::None;
}

void AbstractPirValue::print(std::ostream& out, bool tty) {
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
    if (aliases.count(env))
        env = aliases.at(env);
    fun = fun->followCastsAndForce();
    while (env && env != AbstractREnvironment::UnknownParent) {
        if (envs[env].mkClosures.count(fun))
            return envs[env].mkClosures.at(fun);
        env = envs[env].parentEnv();
    }
    return AbstractREnvironment::UnknownClosure;
}

std::unordered_set<Value*>
AbstractREnvironmentHierarchy::potentialParents(Value* env) const {
    std::unordered_set<Value*> res;
    assert(env);
    if (aliases.count(env))
        env = aliases.at(env);
    while (envs.count(env)) {
        res.insert(env);
        auto aenv = envs.at(env);
        auto parent = envs.at(env).parentEnv();
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
    for (auto e : envs)
        res.insert(e.first);
    return res;
}

AbstractLoad AbstractREnvironmentHierarchy::get(Value* env, SEXP e) const {
    assert(env);
    if (aliases.count(env))
        env = aliases.at(env);
    while (env != AbstractREnvironment::UnknownParent) {
        if (envs.count(env) == 0)
            return AbstractLoad(env ? env : AbstractREnvironment::UnknownParent,
                                AbstractPirValue::tainted());
        auto aenv = envs.at(env);
        if (!aenv.absent(e)) {
            const AbstractPirValue& res = aenv.get(e);
            return AbstractLoad(env, res);
        }
        auto parent = envs.at(env).parentEnv();
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
    if (aliases.count(env))
        env = aliases.at(env);
    if (!envs.count(env))
        return AbstractLoad(AbstractREnvironment::UnknownParent,
                            AbstractPirValue::tainted());
    auto parent = envs.at(env).parentEnv();
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
