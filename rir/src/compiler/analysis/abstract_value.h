#ifndef PIR_ABSTRACT_VALUE_H
#define PIR_ABSTRACT_VALUE_H

#include "../pir/pir.h"

#include <algorithm>
#include <set>
#include <unordered_map>

namespace rir {
namespace pir {

struct AbstractValue {
    bool unknown = false;

    struct ValOrig : public std::pair<Value*, Instruction*> {
        ValOrig(Value* v, Instruction* o)
            : std::pair<Value*, Instruction*>(v, o) {}
        Value* val() const { return first; }
        Instruction* orig() const { return second; }
    };

    std::set<ValOrig> vals;

    PirType type = PirType::bottom();

    AbstractValue();
    AbstractValue(Value* v, Instruction* origin);

    static AbstractValue tainted() {
        AbstractValue v;
        v.taint();
        return v;
    }

    void taint() {
        vals.clear();
        unknown = true;
        type = PirType::any();
    }

    bool isUnknown() const { return unknown; }

    bool singleValue() {
        if (unknown)
            return false;
        return vals.size() == 1;
    }

    bool merge(const AbstractValue& other);

    void print(std::ostream& out = std::cout);
};

static Value* UnknownParent = (Value*)-1;
static Value* UninitializedParent = nullptr;
static Function* UnknownFunction = (Function*)-1;

template <class AV>
struct AbstractEnvironment {
    std::unordered_map<SEXP, AV> entries;
    std::unordered_map<Value*, Function*> functionPointers;

    Value* parentEnv = UninitializedParent;

    bool leaked = false;
    bool tainted = false;

    void taint() {
        tainted = true;
        for (auto& e : entries) {
            e.second.taint();
        }
    }

    void set(SEXP n, Value* v, Instruction* origin) {
        entries[n] = AV(v, origin);
    }

    void print(std::ostream& out = std::cout) {
        for (auto e : entries) {
            SEXP name = std::get<0>(e);
            out << "   " << CHAR(PRINTNAME(name)) << " -> ";
            AV v = std::get<1>(e);
            v.print(out);
            out << "\n";
        }
        out << "\n";
    }

    const AV& get(SEXP e) const {
        static AV t = AV::tainted();
        if (entries.count(e))
            return entries.at(e);
        return t;
    }

    bool merge(AbstractEnvironment& other) {
        bool changed = false;
        if (!leaked && other.leaked)
            changed = leaked = true;
        if (!tainted && other.tainted)
            changed = tainted = true;

        std::set<SEXP> keys;
        for (auto e : entries)
            keys.insert(std::get<0>(e));
        for (auto e : other.entries)
            keys.insert(std::get<0>(e));
        for (auto n : keys) {
            // if this is not the first incomming edge and it has more entries
            // we are in trouble.
            if (!entries.count(n)) {
                entries[n].taint();
                changed = true;
            } else if (entries[n].merge(other.get(n))) {
                changed = true;
            }
        }

        std::set<Value*> fps;
        for (auto e : functionPointers)
            fps.insert(std::get<0>(e));
        for (auto e : other.functionPointers)
            fps.insert(std::get<0>(e));
        for (auto n : fps) {
            if (functionPointers[n] != UnknownFunction &&
                functionPointers[n] != other.functionPointers[n]) {
                functionPointers[n] = UnknownFunction;
            }
        }

        if (parentEnv == UninitializedParent &&
            other.parentEnv != UninitializedParent) {
            parentEnv = other.parentEnv;
            changed = true;
        }

        return changed;
    }
};

template <class AE, class AV>
class AbstractEnvironmentSet : public std::unordered_map<Value*, AE> {
  public:
    typedef std::pair<Value*, AV> AbstractLoadVal;

    bool merge(AbstractEnvironmentSet& other) {
        bool changed = false;
        std::set<Value*> k;
        for (auto e : *this)
            k.insert(e.first);
        for (auto e : other)
            k.insert(e.first);
        for (auto i : k)
            if (this->count(i)) {
                if (this->at(i).merge(other[i]))
                    changed = true;
            } else {
                (*this)[i].taint();
                changed = true;
            }
        return changed;
    }
    void clear() {
        for (auto e : *this)
            e.second.clear();
    }

    Function* findFunction(Value* env, Value* fun) {
        while (env && env != UnknownParent) {
            if ((*this)[env].functionPointers.count(fun))
                return (*this)[env].functionPointers.at(fun);
            env = (*this)[env].parentEnv;
        }
        return UnknownFunction;
    }

    AbstractLoadVal get(Value* env, SEXP e) const {
        while (env != UnknownParent) {
            if (this->count(env) == 0)
                return AbstractLoadVal(env, AV::tainted());
            const AV& res = this->at(env).get(e);
            if (!res.isUnknown())
                return AbstractLoadVal(env, res);
            env = (*this).at(env).parentEnv;
        }
        return AbstractLoadVal(env, AV::tainted());
    }
};
}
}

#endif
