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
        assert(d->info.magic == DISPATCH_TABLE_MAGIC &&
               "This container does not contain a dispatch table.");
        return d;
    }

    static DispatchTable* check(SEXP s) {
        if (TYPEOF(s) != EXTERNALSXP) {
            return nullptr;
        }
        DispatchTable* d = (DispatchTable*)INTEGER(s);
        return d->info.magic == DISPATCH_TABLE_MAGIC ? d : nullptr;
    }

    bool available(size_t i) {
        assert(i < capacity());
        return entry[i];
    }

    Function* at(size_t i) { return Function::unpack(entry[i]); }

    void put(size_t i, Function* f) {
        assert(i < capacity());
        EXTERNALSXP_SET_ENTRY(container(), i, f->container());
    }

    Function* first() {
        assert(info.gc_area_length > 0);
        return Function::unpack(entry[0]);
    }

    Function* getMatching(FunctionSignature* sig) {
        // TODO: Actually do some matching when we have multiple signatures.
        return first();
    }

    rir::rir_header info;

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
    DispatchTable(size_t cap) {
        info.gc_area_start = sizeof(DispatchTable);
        info.gc_area_length = cap;
        info.magic = DISPATCH_TABLE_MAGIC;
        for (size_t c = 0; c < cap; ++c)
            entry[c] = nullptr;
    }

    DispatchTableEntry entry[];
};
#pragma pack(pop)
}

#endif
