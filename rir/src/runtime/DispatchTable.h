#ifndef RIR_DISPATCH_TABLE_H
#define RIR_DISPATCH_TABLE_H

#include "Function.h"
#include "RirHeader.h"

namespace rir {

#define DISPATCH_TABLE_MAGIC (unsigned)0xBEEF1234

typedef SEXP DispatchTableEntry;

/*
 * A dispatch table (vtable) for functions.
 *
 * We set TRUELENGTH to the size of the entry table, i.e. capacity, so the GC
 * knows where the start of the entry table is.
 */
#pragma pack(push)
#pragma pack(1)
struct DispatchTable {
    SEXP container() {
        SEXP result = (SEXP)((uintptr_t) this - sizeof(VECTOR_SEXPREC));
        assert(TYPEOF(result) == EXTERNALSXP &&
               "Dispatch table not embedded in container, or corrupt.");
        return result;
    }

    static DispatchTable* unpack(SEXP s) {
        DispatchTable* d = (DispatchTable*)INTEGER(s);
        assert(d->magic == DISPATCH_TABLE_MAGIC &&
               "This container does not contain a dispatch table.");
        return d;
    }

    static DispatchTable* check(SEXP s) {
        DispatchTable* d = (DispatchTable*)INTEGER(s);
        return d->magic == DISPATCH_TABLE_MAGIC ? d : nullptr;
    }

    Function* at(size_t i) { return Function::unpack(entry[i]); }

    void put(size_t i, Function* f) {
        EXTERNALSXP_SET_ENTRY(container(), i, f->container());
    }

    Function* first() {
        assert(info.gc_area_length > 0);
        return Function::unpack(entry[0]);
    }

    Function* getMatching(FunctionSignature* sig) {
        return first();
    }

    rir::rir_header info;  /// for exposing SEXPs to GC

    uint32_t magic; /// used to detect DispatchTables 0xBEEF1234

    static DispatchTable* create(size_t capacity) {
        size_t size =
            sizeof(DispatchTable) + (capacity * sizeof(DispatchTableEntry));
        SEXP s = Rf_allocVector(EXTERNALSXP, size);
        return new (INTEGER(s)) DispatchTable(capacity);
    }

  private:
    DispatchTable() = delete;
    DispatchTable(size_t cap) {
        magic = DISPATCH_TABLE_MAGIC;
        info.gc_area_start = sizeof(DispatchTable);
        info.gc_area_length = cap;
        for (size_t c = 0; c < cap; ++c)
            entry[c] = nullptr;
    }

    DispatchTableEntry entry[];
};
#pragma pack(pop)
}

#endif
