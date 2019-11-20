#include "../pir/pir_impl.h"
#include "../translations/pir_translator.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/Funtab.h"

#include "../analysis/abstract_value.h"

#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

void TypeInference::apply(RirCompiler&, ClosureVersion* function,
                          LogStream& log) const {

    std::unordered_map<Instruction*, PirType> types;
    {
        bool done = false;
        auto apply = [&]() {
            Visitor::run(function->entry, [&](Instruction* i) {
                if (!i->producesRirResult())
                    return;

                auto getType = [&](Value* v) {
                    if (auto arg = Instruction::Cast(v)) {
                        if (types.count(arg)) {
                            return types.at(arg);
                        } else {
                            done = false;
                        }
                    } else {
                        return v->type;
                    }
                    return PirType::bottom();
                };

                PirType inferred = PirType::bottom();
                switch (i->tag) {
                case Tag::CallSafeBuiltin: {
                    auto c = CallSafeBuiltin::Cast(i);
                    std::string name = getBuiltinName(getBuiltinNr(c->blt));

                    static const std::unordered_set<std::string> bitwise = {
                        "bitwiseXor", "bitwiseShiftL", "bitwiseShiftLR",
                        "bitwiseAnd", "bitwiseNot",    "bitwiseOr"};
                    if (bitwise.count(name)) {
                        inferred = PirType(RType::integer);
                        if (getType(c->arg(0).val()).isScalar() &&
                            getType(c->arg(1).val()).isScalar())
                            inferred.setScalar();
                        break;
                    }

                    if ("length" == name) {
                        inferred =
                            (PirType() | RType::integer | RType::real).scalar();
                        break;
                    }

                    if ("abs" == name) {
                        if (!c->callArg(0).val()->type.maybeObj()) {
                            inferred =
                                c->callArg(0).val()->type & PirType::num();
                        } else {
                            inferred = i->inferType(getType);
                        }
                        break;
                    }

                    if ("as.integer" == name) {
                        inferred =
                            c->callArg(0).val()->type & PirType(RType::integer);
                        break;
                    }

                    if ("typeof" == name) {
                        inferred = PirType(RType::str).scalar();
                        break;
                    }

                    static const std::unordered_set<std::string> vecTests = {
                        "is.na", "is.nan", "is.finite", "is.infinite"};
                    if (vecTests.count(name)) {
                        if (!c->callArg(0).val()->type.maybeObj()) {
                            inferred = PirType(RType::logical);
                            if (getType(c->arg(0).val()).isScalar())
                                inferred.setScalar();
                        } else {
                            inferred = i->inferType(getType);
                        }
                        break;
                    }

                    static const std::unordered_set<std::string> tests = {
                        "is.vector",   "is.null",      "is.integer",
                        "is.double",   "is.complex",   "is.character",
                        "is.symbol",   "is.name",      "is.environment",
                        "is.list",     "is.pairlist",  "is.expression",
                        "is.raw",      "is.object",    "isS4",
                        "is.numeric",  "is.matrix",    "is.array",
                        "is.atomic",   "is.recursive", "is.call",
                        "is.language", "is.function",  "is.single"};
                    if (tests.count(name)) {
                        if (!c->callArg(0).val()->type.maybeObj())
                            inferred = PirType(RType::logical).scalar();
                        else
                            inferred = i->inferType(getType);
                        break;
                    }

                    if ("c" == name) {
                        inferred = i->mergedInputType(getType).collectionType(
                            c->nCallArgs());
                        break;
                    }

                    if ("strsplit" == name) {
                        inferred = RType::vec;
                        break;
                    }

                    inferred = i->inferType(getType);
                    break;
                }

                default:
                    inferred = i->inferType(getType);
                }

                // inference should never generate less precise type
                inferred = inferred & i->type;

                if (!types.count(i) || types.at(i) != inferred) {
                    done = false;
                    types[i] = inferred;
                }
            });
        };
        while (!done) {
            done = true;
            apply();
        }
    }

    Visitor::run(function->entry, [&](Instruction* i) {
        if (!i->producesRirResult())
            return;
        if (types.count(i))
            i->type = types.at(i);
    });
}

} // namespace pir
} // namespace rir
