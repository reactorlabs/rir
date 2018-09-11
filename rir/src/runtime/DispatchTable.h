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
    bool available(size_t i) {
        assert(i < capacity());
        return entry[i];
    }

    Function* at(size_t i) { return Function::unpack(entry[i]); }

    void put(size_t i, Function* f) {
        assert(i < capacity());
        setEntry(i, f->container());
    }

    Function* first() {
        assert(info.gc_area_length > 0);
        return Function::unpack(entry[0]);
    }

    Function* getMatching(FunctionSignature* sig) {
        // TODO: Actually do some matching when we have multiple signatures.
        return first();
    }

    static DispatchTable* create(size_t capacity = 2) {
        // capacity default is 2 for now (rir and pir versions)
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

    DispatchTableEntry entry[];
};
#pragma pack(pop)
}

#endif
