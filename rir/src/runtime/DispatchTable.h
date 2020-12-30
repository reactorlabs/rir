#ifndef RIR_DISPATCH_TABLE_H
#define RIR_DISPATCH_TABLE_H

#include "Function.h"
#include "R/Serialize.h"
#include "RirRuntimeObject.h"

namespace rir {

#define DISPATCH_TABLE_MAGIC (unsigned)0xd7ab1e00

typedef SEXP DispatchTableEntry;

/*
 * A dispatch table (vtable) for functions.
 *
 */
#pragma pack(push)
#pragma pack(1)
struct DispatchTable
    : public RirRuntimeObject<DispatchTable, DISPATCH_TABLE_MAGIC> {

    size_t size() const { return size_; }

    Function* get(size_t i) const {
        assert(i < capacity());
        return Function::unpack(getEntry(i));
    }

    Function* best() const {
        if (size() > 1)
            return get(1);
        return get(0);
    }
    Function* baseline() const {
        auto f = Function::unpack(getEntry(0));
        assert(f->signature().envCreation ==
               FunctionSignature::Environment::CallerProvided);
        return f;
    }

    Function* dispatch(Context a) const {
        if (!a.smaller(userDefinedContext_)) {
#ifdef DEBUG_DISPATCH
            std::cout << "DISPATCH trying: " << a
                      << " vs annotation: " << userDefinedContext_ << "\n";
#endif
            Rf_error("Provided context does not satisfy user defined context");
        }

        for (size_t i = 1; i < size(); ++i) {
#ifdef DEBUG_DISPATCH
            std::cout << "DISPATCH trying: " << a << " vs " << get(i)->context()
                      << "\n";
#endif
            if (a.smaller(get(i)->context()))
                return get(i);
        }
        return baseline();
    }

    void baseline(Function* f) {
        assert(f->signature().optimization ==
               FunctionSignature::OptimizationLevel::Baseline);
        if (size() == 0)
            size_++;
        else
            assert(baseline()->signature().optimization ==
                   FunctionSignature::OptimizationLevel::Baseline);
        setEntry(0, f->container());
    }

    bool contains(const Context& assumptions) const {
        for (size_t i = 0; i < size(); ++i)
            if (get(i)->context() == assumptions)
                return true;
        return false;
    }

    void remove(Code* funCode) {
        size_t i = 1;
        for (; i < size(); ++i) {
            if (get(i)->body() == funCode)
                break;
        }
        if (i == size())
            return;
        get(i)->flags.set(Function::Dead);
        for (; i < size() - 1; ++i) {
            setEntry(i, getEntry(i + 1));
        }
        setEntry(i, nullptr);
        size_--;
    }

    // insert function ordered by increasing number of assumptions
    void insert(Function* fun) {
        // TODO: we might need to grow the DT here!
        assert(size() > 0);
        assert(fun->signature().optimization !=
               FunctionSignature::OptimizationLevel::Baseline);
        auto assumptions = fun->context();
        long i;
        for (i = size() - 1; i > 0; --i) {
            if (get(i)->context() == assumptions) {
                // If we override a version we should ensure that we don't call
                // the old version anymore, or we might end up in a deopt loop.
                if (i != 0) {
                    get(i)->flags.set(Function::Dead);
                    setEntry(i, fun->container());
                    assert(get(i) == fun);
                }
                return;
            }
            if (!(assumptions < get(i)->context())) {
                break;
            }
        }
        i++;
        assert(!contains(fun->context()));
        if (size() == capacity()) {
#ifdef DEBUG_DISPATCH
            std::cout << "Tried to insert into a full Dispatch table. Have: \n";
            for (size_t i = 0; i < size(); ++i) {
                auto e = getEntry(i);
                std::cout << "* " << Function::unpack(e)->context() << "\n";
            }
            std::cout << "\n";
            std::cout << "Tried to insert: " << assumptions << "\n";
            Rf_error("dispatch table overflow");
#endif
            // Evict one element and retry
            auto pos = 1 + (std::rand() % (size() - 1));
            size_--;
            while (pos < size()) {
                setEntry(pos, getEntry(pos + 1));
                pos++;
            }
            return insert(fun);
        }

        for (long j = size(); j > i; --j)
            setEntry(j, getEntry(j - 1));
        size_++;
        setEntry(i, fun->container());

#ifdef DEBUG_DISPATCH
        std::cout << "Added version to DT, new order is: \n";
        for (size_t i = 0; i < size(); ++i) {
            auto e = getEntry(i);
            std::cout << "* " << Function::unpack(e)->context() << "\n";
        }
        std::cout << "\n";
        for (size_t i = 0; i < size() - 1; ++i) {
            assert(get(i)->context() < get(i + 1)->context());
            assert(get(i)->context() != get(i + 1)->context());
            assert(!(get(i + 1)->context() < get(i)->context()));
        }
        assert(contains(fun->context()));
#endif
    }

    static DispatchTable* create(size_t capacity = 20) {
        size_t sz =
            sizeof(DispatchTable) + (capacity * sizeof(DispatchTableEntry));
        SEXP s = Rf_allocVector(EXTERNALSXP, sz);
        return new (INTEGER(s)) DispatchTable(capacity);
    }

    size_t capacity() const { return info.gc_area_length; }

    static DispatchTable* deserialize(SEXP refTable, R_inpstream_t inp) {
        DispatchTable* table = create();
        PROTECT(table->container());
        AddReadRef(refTable, table->container());
        table->size_ = InInteger(inp);
        for (size_t i = 0; i < table->size(); i++) {
            table->setEntry(i,
                            Function::deserialize(refTable, inp)->container());
        }
        UNPROTECT(1);
        return table;
    }

    void serialize(SEXP refTable, R_outpstream_t out) const {
        HashAdd(container(), refTable);
        size_t n = 0;
        for (size_t i = 0; i < size(); i++)
            if (!get(i)->body()->nativeCode)
                n++;
        OutInteger(out, n);
        for (size_t i = 0; i < size(); i++)
            if (!get(i)->body()->nativeCode)
                get(i)->serialize(refTable, out);
    }

    Context userDefinedContext() const { return userDefinedContext_; }
    DispatchTable* newWithUserContext(Context udc) {

        auto clone = create(this->capacity());
        clone->setEntry(0, this->getEntry(0));

        auto j = 1;
        for (size_t i = 1; i < size(); i++) {
            if (get(i)->context().smaller(udc)) {
                clone->setEntry(j, getEntry(i));
                j++;
            }
        }

        clone->size_ = j;
        clone->userDefinedContext_ = udc;
        return clone;
    }

    Context combineContextWith(Context anotherContext) {
        return userDefinedContext_ | anotherContext;
    }

  private:
    DispatchTable() = delete;
    explicit DispatchTable(size_t cap)
        : RirRuntimeObject(
              // GC area starts at the end of the DispatchTable
              sizeof(DispatchTable),
              // GC area is just the pointers in the entry array
              cap) {}

    size_t size_ = 0;
    Context userDefinedContext_;
};
#pragma pack(pop)
} // namespace rir

#endif
