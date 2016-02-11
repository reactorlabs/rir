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

    enum class Attrib : int { Unknown, Absent, Class, Present };

    enum class Size : int { Unknown, Scalar, Any };

    TypeInfo() { *reinterpret_cast<int*>(&store) = 0; }

    TypeInfo(int base) { *reinterpret_cast<int*>(&store) = base; }

    operator int() { return *reinterpret_cast<int*>(&store); }

    const EnumBitset<Type> type() { return EnumBitset<Type>(store.type_); }

    const Attrib attrib() { return store.attrib_; }

    const Size size() { return store.size_; }

    const EnumBitset<Type> addType(int sexpType);

    const EnumBitset<Type> addType(Type type) {
        EnumBitset<Type> b(store.type_);
        b.insert(type);
        store.type_ = b;
        return b;
    }

    void addAttrib(SEXP v);

    void addSize(SEXP v);

  private:
    const void size(Size s) {
        assert(s > Size::Unknown && s <= Size::Any);
        store.size_ = s;
    }

    const void attrib(Attrib a) {
        assert(a > Attrib::Unknown && a <= Attrib::Present);
        store.attrib_ = a;
    }

    friend std::ostream& operator<<(std::ostream& out, TypeInfo& t);

    struct Store {
        int type_ : (int)Type::Any;
        Attrib attrib_ : 3;
        Size size_ : 3;
    };

    static_assert(sizeof(Store) == sizeof(int), "Store must fit into int");

    Store store;
};

std::ostream& operator<<(std::ostream& out, TypeInfo& info);
}

#endif
