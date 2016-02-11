#include "TypeInfo.h"
#include "RIntlns.h"

namespace rjit {

const EnumBitset<TypeInfo::Type> TypeInfo::addType(int sexpType) {
    EnumBitset<TypeInfo::Type> b(store.type_);
    switch (sexpType) {
    case INTSXP:
        b.insert(Type::Integer);
        break;
    case REALSXP:
        b.insert(Type::Float);
        break;
    case STRSXP:
        b.insert(Type::String);
        break;
    case VECSXP:
        b.insert(Type::Vector);
        break;
    case LGLSXP:
        b.insert(Type::Bool);
        break;
    default:
        b.insert(Type::Any);
        break;
    }
    store.type_ = b;
    return b;
}

void TypeInfo::addAttrib(SEXP value) {
    Attrib a = ATTRIB(value) == R_NilValue ? Attrib::Absent : Attrib::Present;
    if (a > attrib())
        attrib(a);
}

void TypeInfo::addSize(SEXP value) {
    Size s = Size::Unknown;
    switch (TYPEOF(value)) {
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case VECSXP:
    case LGLSXP:
        if (XLENGTH(value) == 1)
            s = Size::Scalar;
        else
            s = Size::Any;
    default:
        break;
    }
    if (s > size())
        size(s);
}

std::ostream& operator<<(std::ostream& out, TypeInfo& info) {
    out << "TypeInfo [";

    auto t = info.type();

    out << "(";
    if (t.has(TypeInfo::Type::Any)) {
        out << "any";
    } else {
        if (t.has(TypeInfo::Type::Integer))
            out << "int,";
        if (t.has(TypeInfo::Type::Float))
            out << "float,";
        if (t.has(TypeInfo::Type::String))
            out << "string,";
        if (t.has(TypeInfo::Type::Vector))
            out << "vector,";
    }
    out << ") ";
    switch (info.size()) {
    case TypeInfo::Size::Unknown:
        out << "??";
        break;
    case TypeInfo::Size::Scalar:
        out << "[1]";
        break;
    case TypeInfo::Size::Any:
        out << "[?]";
        break;
    default:
        assert(false);
    }

    out << " ";
    switch (info.attrib()) {
    case TypeInfo::Attrib::Unknown:
        out << "??";
        break;
    case TypeInfo::Attrib::Absent:
        out << "~attr";
        break;
    case TypeInfo::Attrib::Present:
        out << "attr";
        break;
    case TypeInfo::Attrib::Class:
        out << "obj";
        break;
    default:
        assert(false);
    }
    out << "]";

    return out;
}
}
