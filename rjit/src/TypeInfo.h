#ifndef TYPE_INFO
#define TYPE_INFO

#include "EnumBitset.h"
#include <iostream>
#include <cassert>

#include "RDefs.h"

namespace rjit {

/* TypeInfo
 * holds type and shape information about a variable or register
 * Currently it contains: a set of possible types, whether it has attrs and the
 * size class (ie. if its scalar).
 */
class TypeInfo {
  public:
    // -- States to record

    enum class Type : int { Integer, Float, String, Vector, Bool, Any };

    enum class Attrib : uint8_t { Unknown, Absent, Object, Present };

    enum class Size : uint8_t { Unknown, Scalar, Any };

    // -- Constructors

    TypeInfo() {
        store.types_ = EnumBitset<Type>();
        store.size_ = Size::Unknown;
        store.attrib_ = Attrib::Unknown;
    }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"

    TypeInfo(int base) { *reinterpret_cast<int*>(&store) = base; }

    operator int() { return *reinterpret_cast<int*>(&store); }

#pragma GCC diagnostic pop

    // -- getters

    const EnumBitset<Type> types() { return EnumBitset<Type>(store.types_); }

    Attrib attrib() { return store.attrib_; }

    Size size() { return store.size_; }

    // -- setters

    bool hasType(Type t) { return types().has(t) || types().has(Type::Any); }

    const EnumBitset<Type> types(EnumBitset<Type> t) {
        store.types_ = t;
        return t;
    }

    Size size(Size s) {
        assert(s > Size::Unknown && s <= Size::Any);
        store.size_ = s;
        return s;
    }

    Attrib attrib(Attrib a) {
        assert(a > Attrib::Unknown && a <= Attrib::Present);
        store.attrib_ = a;
        return a;
    }

    // -- record a new runtime type instance

    const EnumBitset<Type> addType(int sexpType);
    void mergeAttrib(SEXP v);
    void mergeSize(SEXP v);
    void mergeAll(SEXP s);

    // -- merge two typeinfos

    void mergeTypes(TypeInfo other) { mergeTypes(other.types()); }
    void mergeAttrib(TypeInfo other) { mergeAttrib(other.attrib()); }
    void mergeSize(TypeInfo other) { mergeSize(other.size()); }

  private:
    // -- merge helpers

    void mergeAttrib(Attrib a) {
        if (a > attrib())
            attrib(a);
    }

    void mergeSize(Size s) {
        if (s > size())
            size(s);
    }

    void mergeTypes(EnumBitset<Type> t) { types(t | types()); }

    const EnumBitset<Type> addType(Type type) {
        auto t = types();
        t.insert(type);
        return types(t);
    }

    friend std::ostream& operator<<(std::ostream& out, TypeInfo& t);

    // -- Condensed bitmap store

    // TODO: How can we make Attrib and Size smaller than 8 and stop GCC from
    // whining about it???
    struct Store {
        int types_ : (int)Type::Any + 1;
        Attrib attrib_ : 8;
        Size size_ : 8;
    };

    static_assert(sizeof(Store) == sizeof(int), "Store must fit into int");

    Store store;
};

static_assert(sizeof(TypeInfo) == sizeof(int),
              "Typeinfo cannot be bigger than its Store");

std::ostream& operator<<(std::ostream& out, TypeInfo& info);
}

#endif
