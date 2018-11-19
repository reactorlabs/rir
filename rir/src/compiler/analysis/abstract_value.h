#ifndef PIR_ABSTRACT_VALUE_H
#define PIR_ABSTRACT_VALUE_H

#include "../pir/pir.h"

#include <functional>
#include <set>
#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {
struct ValOrig {
    Value* val;
    Instruction* origin;
    ValOrig(Value* v, Instruction* o) : val(v), origin(o) {}
    bool operator<(const ValOrig& other) const {
        if (origin == other.origin)
            return val < other.val;
        return origin < other.origin;
    }
    bool operator==(const ValOrig& other) const {
        return val == other.val && origin == other.origin;
    }
};
}
}

namespace std {
template <>
struct hash<rir::pir::ValOrig> {
    std::size_t operator()(const rir::pir::ValOrig& v) const {
        using std::hash;
        return hash<rir::pir::Value*>()(v.val) ^
               hash<rir::pir::Instruction*>()(v.origin);
    }
};
}

namespace rir {
namespace pir {

/*
 * Captures an abstract PIR value.
 *
 * Vals is the set of potential candidates. If we don't can't tell what the
 * possible values are, then we set "unknown" (ie. we taint the value). This is
 * the top element of our lattice.
 *
 */
struct AbstractPirValue {
  private:
    bool unknown = false;
    // This needs to be ordered set, for std::includes check!
    std::set<ValOrig> vals;

  public:
    PirType type = PirType::bottom();

    AbstractPirValue();
    AbstractPirValue(Value* v, Instruction* origin);

    static AbstractPirValue tainted() {
        AbstractPirValue v;
        v.taint();
        return v;
    }

    void taint() {
        vals.clear();
        unknown = true;
        type = PirType::any();
    }

    bool isUnknown() const { return unknown; }

    bool isSingleValue() {
        if (unknown)
            return false;
        return vals.size() == 1;
    }

    const ValOrig& singleValue() const {
        assert(vals.size() == 1);
        return *vals.begin();
    }

    typedef std::function<void(Value*)> ValMaybe;
    typedef std::function<void(ValOrig&)> ValOrigMaybe;
    typedef std::function<bool(ValOrig&)> ValOrigMaybePredicate;

    void ifSingleValue(ValMaybe known) {
        if (!unknown && vals.size() == 1)
            known((*vals.begin()).val);
    }

    void eachSource(ValOrigMaybe apply) {
        for (auto v : vals)
            apply(v);
    }

    bool checkEachSource(ValOrigMaybePredicate apply) {
        for (auto v : vals)
            if (!apply(v))
                return false;
        return true;
    }

    bool merge(const ValOrig& other) {
        return merge(AbstractPirValue(other.val, other.origin));
    }

    bool merge(const AbstractPirValue& other);

    void print(std::ostream& out, bool tty = false);
};

/*
 * An AbstractREnvironment is a static approximation of an R runtime Envrionment
 *
 * A key notion is, when an environment leaks. A leaked environment describes
 * an environment, that is visible to an unknown context. This means, that it
 * can be inspected and manipulated by code we can't statically analyze.
 *
 * Typically an analysis will need to mark an environment leaked, when we call
 * a (statically) unknown function. The reason is that the callee can always
 * inspect our environment through sys.parent().
 *
 * For inter-procedural analysis we can additionally keep track of closures.
 */
class MkFunCls;
struct AbstractREnvironment {
    static Value* UnknownParent;
    static Value* UninitializedParent;
    static MkFunCls* UnknownClosure;

    std::unordered_map<SEXP, AbstractPirValue> entries;
    std::unordered_map<Value*, MkFunCls*> mkClosures;

    bool leaked = false;
    bool tainted = false;

    void taint() {
        tainted = true;
        for (auto& e : entries) {
            e.second.taint();
        }
    }

    void set(SEXP n, Value* v, Instruction* origin) {
        entries[n] = AbstractPirValue(v, origin);
    }

    void print(std::ostream& out, bool tty = false);

    const AbstractPirValue& get(SEXP e) const {
        static AbstractPirValue t = AbstractPirValue::tainted();
        if (entries.count(e))
            return entries.at(e);
        return t;
    }

    const bool absent(SEXP e) const { return !tainted && !entries.count(e); }

    bool merge(const AbstractREnvironment& other) {
        bool changed = false;
        if (!leaked && other.leaked)
            changed = leaked = true;
        if (!tainted && other.tainted)
            changed = tainted = true;

        for (auto entry : other.entries) {
            auto name = entry.first;
            if (!entries.count(name)) {
                entries[name].taint();
                changed = true;
            } else if (entries[name].merge(other.get(name))) {
                changed = true;
            }
        }
        for (auto entry : entries) {
            auto name = entry.first;
            if (!other.entries.count(name) && !entries.at(name).isUnknown()) {
                entries.at(name).taint();
                changed = true;
            }
        }

        std::unordered_set<Value*> fps;
        for (auto e : mkClosures)
            fps.insert(std::get<0>(e));
        for (auto e : other.mkClosures)
            fps.insert(std::get<0>(e));
        for (auto n : fps) {
            if (mkClosures[n] != UnknownClosure &&
                (other.mkClosures.count(n) == 0 ||
                 mkClosures[n] != other.mkClosures.at(n))) {
                mkClosures[n] = UnknownClosure;
            }
        }

        if (parentEnv_ == UninitializedParent &&
            other.parentEnv_ != UninitializedParent) {
            parentEnv(other.parentEnv());
            changed = true;
        } else if (parentEnv_ != UninitializedParent &&
                   parentEnv_ != UnknownParent &&
                   other.parentEnv_ != parentEnv_) {
            parentEnv_ = UnknownParent;
            changed = true;
        }

        return changed;
    }

    Value* parentEnv() const {
        if (parentEnv_ == UninitializedParent)
            return UnknownParent;
        return parentEnv_;
    }

    void parentEnv(Value* v) {
        assert(v);
        parentEnv_ = v;
    }

  private:
    Value* parentEnv_ = UninitializedParent;
};

/*
 * AbstractEnvironmentSet is an abstract domain that deals with multiple
 * environments at the same time. This is necessary for inter-procedural
 * analysis, or analyzing a function with multiple environments.
 *
 */

struct AbstractLoad {
    Value* env;
    AbstractPirValue result;

    AbstractLoad(Value* env, const AbstractPirValue& val)
        : env(env), result(val) {
        assert(env);
    }
};

class AbstractREnvironmentHierarchy {
  private:
    std::unordered_map<Value*, AbstractREnvironment> envs;

  public:
    std::unordered_map<Value*, Value*> aliases;

    bool merge(const AbstractREnvironmentHierarchy& other) {
        bool changed = false;
        std::unordered_set<Value*> k;
        for (auto e : envs)
            k.insert(e.first);
        for (auto e : other.envs)
            k.insert(e.first);
        for (auto i : k)
            if (envs.count(i)) {
                if (other.envs.count(i) == 0) {
                    if (!envs.at(i).tainted) {
                        changed = true;
                        envs.at(i).taint();
                    }
                } else if (envs.at(i).merge(other.envs.at(i))) {
                    changed = true;
                }
            } else {
                envs[i].taint();
                changed = true;
            }
        for (auto& a : other.aliases) {
            if (!aliases.count(a.first)) {
                aliases.emplace(a);
                changed = true;
            } else {
                SLOWASSERT(a.second == aliases.at(a.first));
            }
        }
        return changed;
    }

    AbstractREnvironment& operator[](Value* env) {
        if (aliases.count(env))
            return envs[aliases.at(env)];
        else
            return envs[env];
    }

    void print(std::ostream& out, bool tty = false);

    MkFunCls* findClosure(Value* env, Value* fun);

    AbstractLoad get(Value* env, SEXP e) const;
    AbstractLoad superGet(Value* env, SEXP e) const;

    std::unordered_set<Value*> potentialParents(Value* env) const;
};
}
}

#endif
