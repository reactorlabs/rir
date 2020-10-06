#include "Context.h"
#include "R/Serialize.h"
#include "compiler/compiler.h"
#include "compiler/pir/closure.h"
#include "compiler/pir/closure_version.h"

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

        TYPE_ASSUMPTIONS(Eager, "Eager");
        TYPE_ASSUMPTIONS(NotObj, "!Obj");
        TYPE_ASSUMPTIONS(SimpleInt, "SimpleInt");
        TYPE_ASSUMPTIONS(SimpleReal, "SimpleReal");
        TYPE_ASSUMPTIONS(NonRefl, "NonRefl");
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
constexpr std::array<TypeAssumption, Context::NUM_TYPED_ARGS>
    Context::NonReflContext;

void Context::setSpecializationLevel(int level) {
    static Flags preserve =
        pir::Compiler::minimalContext | Assumption::StaticallyArgmatched;

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

bool Context::isImproving(Function* f) const {
    return isImproving(f->context(), f->signature().hasDotsFormals,
                       f->signature().hasDefaultArgs);
}
bool Context::isImproving(pir::ClosureVersion* f) const {
    return isImproving(f->context(), f->owner()->formals().hasDots(),
                       f->owner()->formals().hasDefaultArgs());
}

bool Context::isImproving(const Context& other, bool hasDotsFormals,
                          bool hasDefaultArgs) const {
    assert(smaller(other));

    if (other == *this)
        return false;
    auto normalized = *this;

    if (!hasDotsFormals)
        normalized.remove(Assumption::StaticallyArgmatched);
    if (!hasDefaultArgs)
        normalized.remove(Assumption::NoExplicitlyMissingArgs);

    // These don't pay of that much...
    normalized.clearObjFlags();

    if (hasDotsFormals || hasDefaultArgs) {
        if (normalized.numMissing() != other.numMissing())
            return true;
    } else {
        normalized.numMissing(other.numMissing());
    }

    normalized = normalized | other;
    return normalized != other;
}

} // namespace rir
