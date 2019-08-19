#include "type_signature.h"
#include <regex>

namespace rir {
namespace pir {

TypeSignature TypeSignature::any() { return TypeSignature(Form::Any); }

TypeSignature TypeSignature::nothing() { return TypeSignature(Form::Nothing); }

TypeSignature TypeSignature::parse(const std::string& inp) {
    auto fail = []() {
        Rf_error("couldn't parse PIR signature: must be of the form <type>* -> "
                 "<type>");
    };

    std::smatch match;
    if (!std::regex_match(inp, match,
                          std::regex("^\\s*(.*)->\\s*([^ ]+)\\s*$")))
        fail();
    std::string argsInp = match[1];
    PirType retType = PirType::parse(match[2]);

    std::vector<PirType> argTypes;
    while (std::regex_search(argsInp, match, std::regex("([^ ]+)\\s*"))) {
        argTypes.push_back(PirType::parse(match[1]));
        argsInp = match.suffix();
    }

    if (!argsInp.empty())
        fail();

    return TypeSignature(argTypes, retType);
}

TypeSignature::TypeSignature(std::vector<PirType> argsVec, PirType result)
    : form(Form::Regular), args(), numArgs(argsVec.size()), result(result) {
    if (argsVec.size() >= TypeSignature::MAX_NUM_ARGS) {
        Rf_error("pir signatures can only take up to %d args",
                 TypeSignature::MAX_NUM_ARGS);
    }
    for (unsigned i = 0; i < argsVec.size(); i++) {
        args[i] = argsVec[i];
    }
}

bool TypeSignature::accepts(std::vector<PirType> inArgs) const {
    switch (form) {
    case Form::Regular:
        if (numArgs != inArgs.size())
            return false;
        for (unsigned i = 0; i < numArgs; i++) {
            PirType arg = args[i];
            PirType inArg = inArgs[i];
            if (!inArg.isA(arg))
                return false;
        }
        return true;
    case Form::Any:
        return true;
    case Form::Nothing:
        return false;
    default:
        assert(false);
        return false;
    }
}

TypeSignature TypeSignature::operator|(const TypeSignature& other) const {
    if (isAny() || other.isNothing())
        return other;
    else if (isNothing() || other.isAny())
        return *this;
    else if (numArgs != other.numArgs)
        return TypeSignature::nothing();
    std::vector<PirType> resArgs;
    for (unsigned i = 0; i < numArgs; i++) {
        resArgs.push_back(args[i] & other.args[i]);
    }
    return TypeSignature(resArgs, result | other.result);
}

void TypeSignature::print(std::ostream& out) const {
    switch (form) {
    case Form::Regular:
        for (unsigned i = 0; i < numArgs; i++) {
            PirType arg = args[i];
            out << arg << " ";
        }
        out << "-> " << result;
        break;
    case Form::Any:
        out << "[any]";
        break;
    case Form::Nothing:
        out << "[nothing]";
        break;
    default:
        assert(false);
        break;
    }
}

std::ostream& operator<<(std::ostream& out, const TypeSignature& s) {
    s.print(out);
    return out;
}

} // namespace pir
} // namespace rir