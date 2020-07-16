#include "Context.h"
#include "R/Serialize.h"
#include "compiler/translations/rir_2_pir/rir_2_pir_compiler.h"

namespace rir {

Context Context::deserialize(SEXP refTable, R_inpstream_t inp) {
    Context as;
    InBytes(inp, &as, sizeof(Context));
    return as;
}

void Context::serialize(SEXP refTable, R_outpstream_t out) const {
    OutBytes(out, this, sizeof(Context));
}

std::ostream& operator<<(std::ostream& out, Assumption a) {
    switch (a) {
    case Assumption::NoReflectiveArgument:
        out << "!RefA";
        break;
    case Assumption::NoExplicitlyMissingArgs:
        out << "!ExpMi";
        break;
    case Assumption::CorrectOrderOfArguments:
        out << "CorrOrd";
        break;
    case Assumption::StaticallyArgmatched:
        out << "Argmatch";
        break;
    case Assumption::NotTooManyArguments:
        out << "!TMany";
        break;
    }
    return out;
};

std::ostream& operator<<(std::ostream& out, TypeAssumption a) {
    switch (a) {
#define TYPE_ASSUMPTIONS(Type, Msg)                                            \
    case TypeAssumption::Arg0Is##Type##_:                                      \
        out << Msg << "0";                                                     \
        break;                                                                 \
    case TypeAssumption::Arg1Is##Type##_:                                      \
        out << Msg << "1";                                                     \
        break;                                                                 \
    case TypeAssumption::Arg2Is##Type##_:                                      \
        out << Msg << "2";                                                     \
        break;                                                                 \
    case TypeAssumption::Arg3Is##Type##_:                                      \
        out << Msg << "3";                                                     \
        break;                                                                 \
    case TypeAssumption::Arg4Is##Type##_:                                      \
        out << Msg << "4";                                                     \
        break;                                                                 \
    case TypeAssumption::Arg5Is##Type##_:                                      \
        out << Msg << "5";                                                     \
        break;                                                                 \
    case TypeAssumption::Arg6Is##Type##_:                                      \
        out << Msg << "6";                                                     \
        break;                                                                 \
    case TypeAssumption::Arg7Is##Type##_:                                      \
        out << Msg << "7";                                                     \
        break;

        TYPE_ASSUMPTIONS(Eager, "Eager");
        TYPE_ASSUMPTIONS(NotObj, "!Obj");
        TYPE_ASSUMPTIONS(SimpleInt, "SimpleInt");
        TYPE_ASSUMPTIONS(SimpleReal, "SimpleReal");
    }
    return out;
};

std::ostream& operator<<(std::ostream& out, const Context& a) {
    for (auto i = a.flags.begin(); i != a.flags.end(); ++i) {
        out << *i;
        if (i + 1 != a.flags.end())
            out << ",";
    }
    if (!a.typeFlags.empty())
        out << ";";
    for (auto i = a.typeFlags.begin(); i != a.typeFlags.end(); ++i) {
        out << *i;
        if (i + 1 != a.typeFlags.end())
            out << ",";
    }
    if (a.missing > 0)
        out << " miss: " << (int)a.missing;
    return out;
}

constexpr std::array<TypeAssumption, Context::NUM_TYPED_ARGS>
    Context::EagerContext;
constexpr std::array<TypeAssumption, Context::NUM_TYPED_ARGS>
    Context::NotObjContext;
constexpr std::array<TypeAssumption, Context::NUM_TYPED_ARGS>
    Context::SimpleIntContext;
constexpr std::array<TypeAssumption, Context::NUM_TYPED_ARGS>
    Context::SimpleRealContext;

void Context::setSpecializationLevel(int level) {
    static Flags preserve =
        pir::Rir2PirCompiler::minimalContext | Assumption::StaticallyArgmatched;

    switch (level) {
    // All Specialization Disabled
    case 0:
        flags = flags & preserve;
        typeFlags.reset();
        missing = 0;
        break;

    // Eager Args
    case 1:
        flags = flags & preserve;
        typeFlags = typeFlags & allEagerArgsFlags();
        missing = 0;
        break;

    // + not Reflective
    case 2:
        flags.reset(Assumption::NoExplicitlyMissingArgs);
        typeFlags = typeFlags & allEagerArgsFlags();
        missing = 0;
        break;

    // + not Object
    case 3:
        flags.reset(Assumption::NoExplicitlyMissingArgs);
        typeFlags = typeFlags & (allEagerArgsFlags() | allNonObjArgsFlags());
        missing = 0;
        break;

    // + arg Types
    case 4:
        flags.reset(Assumption::NoExplicitlyMissingArgs);
        missing = 0;
        break;

    // + nargs
    case 5:
        flags.reset(Assumption::NoExplicitlyMissingArgs);
        break;

    // + no explicitly missing
    // all
    default:
        break;
    }
}

} // namespace rir
