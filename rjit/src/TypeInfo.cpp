#include "TypeInfo.h"
#include "RIntlns.h"

namespace rjit {

void TypeInfo::mergeAll(SEXP value) {
    addType(value);
    mergeAttrib(value);
    mergeSize(value);
}

const EnumBitset<TypeInfo::Type> TypeInfo::addType(SEXP value) {
    int sexpType = TYPEOF(value);
    Type t = Type::Any;
    switch (sexpType) {
    case INTSXP:
        t = Type::Integer;
        break;
    case REALSXP:
        t = Type::Float;
        break;
    case STRSXP:
        t = Type::String;
        break;
    case VECSXP:
        t = Type::Vector;
        break;
    case LGLSXP:
        t = Type::Bool;
        break;
    default:
        break;
    }
    addType(t);
    assert(!types().empty());
    return types();
}

void TypeInfo::mergeAttrib(SEXP value) {
    // TODO: find out if value is an object
    Attrib a = ATTRIB(value) == R_NilValue ? Attrib::Absent : Attrib::Any;
    mergeAttrib(a);
}

void TypeInfo::mergeSize(SEXP value) {
    Size s = Size::Any;
    switch (TYPEOF(value)) {
    case REALSXP:
    case STRSXP:
    case VECSXP:
        if (XLENGTH(value) == 1)
            s = Size::Scalar;
        break;
    case INTSXP:
        // TODO: we need na overflow check to make this possible
        // if (XLENGTH(value) == 1)
        //     s = Size::Scalar;
        break;
    case LGLSXP:
        if (XLENGTH(value) == 1)
            s = Size::Scalar;
        break;
    default:
        break;
    }
    mergeSize(s);
}

std::ostream& operator<<(std::ostream& out, TypeInfo& info) {
    auto t = info.types();
    out << "[(";
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
        if (t.has(TypeInfo::Type::Bool))
            out << "bool,";
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
        out << "~";
        break;
    case TypeInfo::Attrib::Any:
        out << "attr";
        break;
    case TypeInfo::Attrib::Object:
        out << "obj";
        break;
    default:
        assert(false);
    }
    out << "]";

    return out;
}
}
