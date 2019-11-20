#ifndef PIR_ABSTRACT_VALUE_H
#define PIR_ABSTRACT_VALUE_H

#include "../pir/pir.h"
#include "../pir/singleton_values.h"
#include "abstract_result.h"
#include "utils/Map.h"
#include "utils/Set.h"

#include <functional>
#include <set>
#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

struct ValOrig {
    Value* val;
    Instruction* origin;
    unsigned recursionLevel;

    ValOrig(Value* v, Instruction* o, unsigned recursionLevel)
        : val(v), origin(o), recursionLevel(recursionLevel) {}

    bool operator<(const ValOrig& other) const {
        if (origin == other.origin && recursionLevel == other.recursionLevel)
            return val < other.val;
        if (origin == other.origin)
            return recursionLevel < other.recursionLevel;
        return origin < other.origin;
    }
    bool operator==(const ValOrig& other) const {
        return val == other.val && origin == other.origin &&
               recursionLevel == other.recursionLevel;
    }
    bool operator!=(const ValOrig& other) const { return !(*this == other); }
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
    SmallSet<ValOrig> vals;
    constexpr static size_t MAX_VALS = 5;

  public:
    PirType type = PirType::bottom();

    AbstractPirValue();

    AbstractPirValue(Value* v, Instruction* origin, unsigned recursionLevel);

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

    bool isSingleValue() const {
        if (unknown)
            return false;
        return vals.size() == 1;
    }

    const ValOrig& singleValue() const {
        assert(vals.size() == 1);
        return *vals.begin();
    }

    typedef std::function<void(Value*)> ValMaybe;
    typedef std::function<void(const ValOrig&)> ValOrigMaybe;
    typedef std::function<bool(const ValOrig&)> ValOrigMaybePredicate;

    void ifSingleValue(ValMaybe known) const {
        if (!unknown && vals.size() == 1)
            known((*vals.begin()).val);
    }

    void eachSource(const ValOrigMaybe& apply) const {
        for (auto& v : vals)
            apply(v);
    }

    bool checkEachSource(const ValOrigMaybePredicate& apply) const {
        for (auto& v : vals)
            if (!apply(v))
                return false;
        return true;
    }

    bool isUnboundValue() const {
        if (unknown)
            return false;
        if (vals.empty())
            return false;
        for (auto& v : vals)
            if (v.val != UnboundValue::instance())
                return false;
        return true;
    }

    bool maybeUnboundValue() const {
        if (unknown)
            return true;
        if (vals.empty())
            return false;
        for (auto& v : vals)
            if (v.val == UnboundValue::instance())
                return true;
        return false;
    }

    AbstractResult merge(const ValOrig& other) {
        return merge(
            AbstractPirValue(other.val, other.origin, other.recursionLevel));
    }

    AbstractResult merge(const AbstractPirValue& other);
    AbstractResult mergeExit(const AbstractPirValue& other) {
        return merge(other);
    }

    void print(std::ostream& out, bool tty = false) const;

    bool operator==(const AbstractPirValue& other) const {
        if (unknown && other.unknown)
            return true;
        return type == other.type && vals == other.vals &&
               unknown == other.unknown;
    }

    bool operator!=(const AbstractPirValue& other) const {
        return !(*this == other);
    }
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
struct AbstractREnvironment {
    explicit AbstractREnvironment(const AbstractREnvironment& other) = default;

    static Value* UnknownParent;
    static Value* UninitializedParent;

    SmallMap<SEXP, AbstractPirValue> entries;
    SmallSet<Value*> reachableEnvs;

    AbstractREnvironment() {}

    bool tainted = false;

    void taint() {
        tainted = true;
        for (auto& e : entries) {
            e.second.taint();
        };
    }

    void set(SEXP n, Value* v, Instruction* origin, unsigned recursionLevel) {
        entries.set(n, AbstractPirValue(v, origin, recursionLevel));
    }

    void print(std::ostream& out, bool tty = false) const;

    const AbstractPirValue& get(SEXP e) const {
        static AbstractPirValue t = AbstractPirValue::tainted();
        return entries.get(e, t);
    }

    const bool absent(SEXP e) const { return !tainted && !entries.contains(e); }

    AbstractResult mergeExit(const AbstractREnvironment& other) {
        return merge(other);
    }

    void leak() { leaked_ = true; }
    bool leaked() const { return leaked_; }

    AbstractResult merge(const AbstractREnvironment& other) {
        AbstractResult res;

        if (!leaked_ && other.leaked_) {
            leaked_ = true;
            res.lostPrecision();
        }
        if (!tainted && other.tainted) {
            tainted = true;
            res.taint();
        }

        for (const auto& entry : other.entries) {
            auto name = entry.first;
            entries.contains(name,
                             [&](AbstractPirValue& val) {
                                 res.max(val.merge(entry.second));
                             },
                             [&]() {
                                 AbstractPirValue copy = entry.second;
                                 res.max(copy.merge(AbstractPirValue(
                                     UnboundValue::instance(), nullptr, 0)));
                                 entries.insert(name, copy);
                                 res.update();
                             });
        }
        for (auto& entry : entries) {
            auto name = entry.first;
            if (!entry.second.isUnknown() && !other.entries.contains(name)) {
                res.max(entry.second.merge(
                    AbstractPirValue(UnboundValue::instance(), nullptr, 0)));
            }
        };
        for (auto& e : other.reachableEnvs) {
            if (!reachableEnvs.count(e)) {
                reachableEnvs.insert(e);
                res.update();
            }
        }

        if (parentEnv_ == UninitializedParent &&
            other.parentEnv_ != UninitializedParent) {
            parentEnv(other.parentEnv());
            res.update();
        } else if (parentEnv_ != UninitializedParent &&
                   parentEnv_ != UnknownParent &&
                   other.parentEnv_ != parentEnv_) {
            parentEnv_ = UnknownParent;
            res.lostPrecision();
        }

        return res;
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
    bool leaked_ = false;
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

    explicit AbstractLoad(const AbstractPirValue& val)
        : env(AbstractREnvironment::UnknownParent), result(val) {}

    AbstractLoad(Value* env, const AbstractPirValue& val)
        : env(env), result(val) {
        assert(env);
    }

    bool operator==(const AbstractLoad& other) const {
        return env == other.env && result == other.result;
    }

    bool operator!=(const AbstractLoad& other) const {
        return !(*this == other);
    }
};

class AbstractREnvironmentHierarchy {
  private:
    SmallMap<Value*, AbstractREnvironment> envs;

  public:
    explicit AbstractREnvironmentHierarchy(
        const AbstractREnvironmentHierarchy& other) = default;

    AbstractREnvironmentHierarchy() {}

    SmallMap<Value*, Value*> aliases;

    AbstractResult mergeExit(const AbstractREnvironmentHierarchy& other) {
        return merge(other);
    }

    AbstractResult merge(const AbstractREnvironmentHierarchy& other) {
        AbstractResult res;

        for (const auto& e : other.envs)
            envs.contains(e.first,
                          [&](AbstractREnvironment& env) {
                              res.max(env.merge(e.second));
                          },
                          [&]() { envs.insert(e.first, e.second); });

        for (auto& entry : other.aliases) {
            if (!aliases.count(entry.first)) {
                aliases.insert(entry.first, entry.second);
                res.update();
            } else {
                SLOWASSERT(entry.second == aliases.at(entry.first));
            }
        }
        return res;
    }

    bool known(Value* env) const { return envs.contains(env); }

    const AbstractREnvironment& at(Value* env) const {
        if (aliases.count(env))
            return envs.at(aliases.at(env));
        else
            return envs.at(env);
    }

    AbstractREnvironment& at(Value* env) {
        if (aliases.count(env))
            return envs[aliases.at(env)];
        else
            return envs[env];
    }

    void addDependency(Value* from, Value* to);

    void leak(Value* env) {
        auto& ae = at(env);
        if (ae.leaked())
            return;

        ae.leak();
        for (auto& r : ae.reachableEnvs)
            leak(r);
    }

    void print(std::ostream& out, bool tty = false) const;

    AbstractLoad get(Value* env, SEXP e) const;
    AbstractLoad getFun(Value* env, SEXP e) const;
    AbstractLoad superGet(Value* env, SEXP e) const;

    std::unordered_set<Value*> potentialParents(Value* env) const;

    AbstractResult taintLeaked() {
        AbstractResult res;
        for (auto& e : envs) {
            if (e.second.leaked()) {
                e.second.taint();
                res.taint();
            }
        }
        return res;
    }
};

template <typename Kind>
class AbstractUnique {
    Kind* val = nullptr;

  public:
    AbstractUnique() {}

    void set(Kind* val_) {
        assert(val_);
        val = val_;
    }

    virtual void clear() { val = nullptr; }

    Kind* get() const { return val; }

    AbstractResult mergeExit(const AbstractUnique& other) {
        return merge(other);
    }
    AbstractResult merge(const AbstractUnique& other) {
        if (val && val != other.val) {
            val = nullptr;
            return AbstractResult::Updated;
        }
        return AbstractResult::None;
    }

    void print(std::ostream& out, bool tty) const {
        if (val)
            val->printRef(out);
        else
            out << "?";
        out << "\n";
    };
};

template <typename T>
struct IntersectionSet {
    SmallSet<T> available;

    AbstractResult mergeExit(const IntersectionSet& other) {
        return merge(other);
    }

    AbstractResult merge(const IntersectionSet& other) {
        AbstractResult res;
        for (auto it = available.cbegin(); it != available.cend();) {
            if (other.available.includes(*it)) {
                it++;
            } else {
                it = available.erase(it);
                res.update();
            }
        }
        return res;
    }

    void print(std::ostream& out, bool tty) {
        for (auto& a : available) {
            a.print(out, tty);
            out << " ";
        }
        out << "\n";
    }
};
}
}

#endif
