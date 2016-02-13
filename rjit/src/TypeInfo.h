#ifndef TYPE_INFO
#define TYPE_INFO

#include "EnumBitset.h"
#include <iostream>
#include <cassert>

#include "RDefs.h"

namespace rjit {

class TypeInfo {
  public:
    enum class Type : int { Integer, Float, String, Vector, Bool, Any };

    enum class Attrib : uint8_t { Unknown, Absent, Object, Present };

    enum class Size : uint8_t { Unknown, Scalar, Any };

    TypeInfo() {
        store.types_ = EnumBitset<Type>();
        store.size_ = Size::Unknown;
        store.attrib_ = Attrib::Unknown;
    }

    TypeInfo(int base) { *reinterpret_cast<int*>(&store) = base; }

    operator int() { return *reinterpret_cast<int*>(&store); }

    const EnumBitset<Type> types() { return EnumBitset<Type>(store.types_); }

    const Attrib attrib() { return store.attrib_; }

    const Size size() { return store.size_; }

    const EnumBitset<Type> addType(int sexpType);

    const EnumBitset<Type> addType(Type type) {
        auto t = types();
        t.insert(type);
        return types(t);
    }

    void mergeTypes(EnumBitset<Type> t) { types(t | types()); }
    void mergeTypes(TypeInfo other) { mergeTypes(other.types()); }

    bool hasType(Type t) { return types().has(t) || types().has(Type::Any); }

    void mergeAttrib(SEXP v);
    void mergeAttrib(Attrib a) {
        if (a > attrib())
            attrib(a);
    }
    void mergeAttrib(TypeInfo other) { mergeAttrib(other.attrib()); }

    void mergeSize(SEXP v);
    void mergeSize(Size s) {
        if (s > size())
            size(s);
    }
    void mergeSize(TypeInfo other) { mergeSize(other.size()); }

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

  private:
    friend std::ostream& operator<<(std::ostream& out, TypeInfo& t);

    struct Store {
        int types_ : (int)Type::Any + 1;
        Attrib attrib_ : 8;
        Size size_ : 8;
    };

    static_assert(sizeof(Store) == sizeof(int), "Store must fit into int");

    Store store;
};

std::ostream& operator<<(std::ostream& out, TypeInfo& info);
}

#endif
