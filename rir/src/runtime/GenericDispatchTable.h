#pragma once

#include "RirRuntimeObject.h"
#include "utils/random.h"

#define GENERIC_DISPATCH_TABLE_MAGIC (unsigned)0xd7ab1e09

namespace rir {

/*
 * A generic dispatch table implementation. Keys must support "operator<" for
 * insertion and "smaller" for dispatch. Values must be PirRuntimeObjects.
 *
 * DataLayout for capacity = n:
 *
 *   key_1
 *   ...
 *   key_n
 *   SEXP_1
 *   ...
 *   SEXP_n
 *
 */

template <typename Key, typename Value, size_t CAPACITY>
struct GenericDispatchTable
    : public RirRuntimeObject<GenericDispatchTable<Key, Value, CAPACITY>,
                              GENERIC_DISPATCH_TABLE_MAGIC> {
  public:
    typedef RirRuntimeObject<GenericDispatchTable<Key, Value, CAPACITY>,
                             GENERIC_DISPATCH_TABLE_MAGIC>
        Super;

    using Super::getEntry;
    using Super::info;
    using Super::setEntry;

    size_t size() const {
        assert(size_ <= capacity());
        return size_;
    }
    size_t capacity() const { return info.gc_area_length; }

    static GenericDispatchTable* create() {
        size_t sz = sizeof(GenericDispatchTable) + (CAPACITY * sizeof(SEXP));
        SEXP s = Rf_allocVector(EXTERNALSXP, sz);
        return new (INTEGER(s)) GenericDispatchTable();
    }

    // insert function ordered by increasing number of assumptions
    void insert(const Key& k, Value* value) {
        size_t i = 0;
        if (size() > 0) {
            for (i = size() - 1; i > 0; --i) {
                if (key(i) == k) {
                    if (i != 0) {
                        setEntry(i, value->container());
                    }
                    return;
                }
                if (!(k < key(i))) {
                    i++;
                    break;
                }
            }
        }
        if (size() == capacity()) {
            // Evict one element and retry
            size_t pos = (Random::singleton()() % (size() - 1));
            size_--;
            while (pos < size()) {
                key(pos) = key(pos + 1);
                setEntry(pos, getEntry(pos + 1));
                pos++;
            }
            return insert(k, value);
        }

        for (size_t j = size(); j > i; --j) {
            key(j) = key(j - 1);
            setEntry(j, getEntry(j - 1));
        }
        size_++;
        key(i) = k;
        setEntry(i, value->container());
    }

    std::pair<const Key&, Value*> dispatch(const Key& a) const {
        for (size_t i = 0; i < size(); ++i) {
            if (a.smaller(key(i))) {
                auto v = Value::unpack(getEntry(i));
                if (!v->disabled())
                    return {key(i), Value::unpack(getEntry(i))};
            }
        }
        return {a, nullptr};
    }

    bool full() const { return size() == capacity(); }

  private:
    GenericDispatchTable()
        : Super(
              // GC area starts at the end of the DispatchTable
              sizeof(GenericDispatchTable),
              // GC area is just the pointers in the entry array
              CAPACITY) {}

    size_t size_ = 0;
    std::array<Key, CAPACITY> keys;

    Key& key(size_t i) {
        assert(i <= size() && i < capacity());
        return keys.at(i);
    };
    const Key& key(size_t i) const {
        assert(i < size());
        return keys.at(i);
    };
};

} // namespace rir
