#ifndef RIR_DISPATCH_TABLE_H
#define RIR_DISPATCH_TABLE_H

#include "Function.h"
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

    size_t size() { return size_; }

    Function* get(size_t i) {
        assert(i < capacity());
        return Function::unpack(getEntry(i));
    }

    Function* baseline() { return Function::unpack(getEntry(0)); }

    void baseline(Function* f) {
        assert(f->signature().optimization ==
               FunctionSignature::OptimizationLevel::Baseline);
        setEntry(0, f->container());
        if (size() == 0)
            size_++;
    }

    bool contains(const Assumptions& assumptions) {
        for (size_t i = 1; i < size(); ++i)
            if (get(i)->signature().assumptions == assumptions)
                return true;
        return false;
    }

    // insert function ordered by increasing number of assumptions
    void insert(Function* fun) {
        // TODO: we might need to grow the DT here!
        assert(size() > 0);
        assert(fun->signature().optimization !=
               FunctionSignature::OptimizationLevel::Baseline);
        auto assumptions = fun->signature().assumptions;
        assert(size() < capacity());
        size_t i = 1;
        for (; i < size(); ++i) {
            if (get(i)->signature().assumptions == assumptions) {
                setEntry(i, fun->container());
                return;
            }
            if (!(assumptions < get(i)->signature().assumptions)) {
                break;
            }
        }
        SLOWASSERT(!contains(fun->signature().assumptions));
        size_++;
        for (size_t j = size() - 1; j > i; --j) {
            setEntry(j, getEntry(j - 1));
        }
        setEntry(i, fun->container());

#ifdef DEBUG_DISPATCH
        std::cout << "Added version to DT, new order is: \n";
        for (size_t i = 0; i < size(); ++i) {
            auto e = getEntry(i);
            std::cout << "* " << Function::unpack(e)->signature().assumptions
                      << "\n";
        }
        std::cout << "\n";
#endif
        SLOWASSERT(contains(fun->signature().assumptions));
    }

    static DispatchTable* create(size_t capacity = 31) {
        size_t size =
            sizeof(DispatchTable) + (capacity * sizeof(DispatchTableEntry));
        SEXP s = Rf_allocVector(EXTERNALSXP, size);
        return new (INTEGER(s)) DispatchTable(capacity);
    }

    size_t capacity() const { return info.gc_area_length; }

  private:
    DispatchTable() = delete;
    explicit DispatchTable(size_t cap)
        : RirRuntimeObject(
              // GC area starts at the end of the DispatchTable
              sizeof(DispatchTable),
              // GC area is just the pointers in the entry array
              cap) {}

    size_t size_ = 0;
};
#pragma pack(pop)
} // namespace rir

#endif
