#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "compiler/analysis/cfg.h"

#include "../analysis/abstract_value.h"
#include "../analysis/range.h"

#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

bool TypeInference::apply(Compiler&, ClosureVersion* cls, Code* code,
                          LogStream& log) const {

    RangeAnalysis rangeAnalysis(cls, code, log);

    std::unordered_map<Instruction*, PirType> types;
    {
        bool done = false;
        auto apply = [&]() {
            Visitor::run(code->entry, [&](Instruction* i) {
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
                        if (getType(c->callArg(0).val()).isScalar() &&
                            getType(c->callArg(1).val()).isScalar())
                            inferred.setScalar();
                        break;
                    }

                    if ("length" == name) {
                        inferred =
                            (PirType() | RType::integer | RType::real).scalar();
                        break;
                    }

                    int doSummary = "min" == name || "max" == name ||
                                    "prod" == name || "sum" == name;
                    if (name == "abs" || doSummary) {
                        if (c->nCallArgs()) {
                            auto m = PirType::bottom();
                            for (size_t i = 0; i < c->nCallArgs(); ++i)
                                m = m.mergeWithConversion(
                                    getType(c->callArg(i).val()));
                            if (!m.maybeObj()) {
                                inferred = m & PirType::num();

                                if (inferred.maybe(RType::logical))
                                    inferred = inferred.orT(RType::integer)
                                                   .notT(RType::logical);

                                if (doSummary)
                                    inferred.setScalar();
                                if ("prod" == name)
                                    inferred = inferred.orT(RType::real)
                                                   .notT(RType::integer);
                                break;
                            }
                        }
                    }

                    if ("sqrt" == name) {
                        if (c->nCallArgs()) {
                            auto m = PirType::bottom();
                            for (size_t i = 0; i < c->nCallArgs(); ++i)
                                m = m.mergeWithConversion(
                                    getType(c->callArg(i).val()));
                            if (!m.maybeObj()) {
                                inferred = m & PirType::num();
                                inferred = inferred.orT(RType::real)
                                               .notT(RType::integer);
                                break;
                            }
                        }
                    }

                    if ("as.integer" == name) {
                        if (!getType(c->callArg(0).val()).maybeObj()) {
                            inferred = PirType(RType::integer);
                            if (getType(c->callArg(0).val()).isScalar())
                                inferred.setScalar();
                        } else {
                            inferred = i->inferType(getType);
                        }
                        break;
                    }

                    if ("typeof" == name) {
                        inferred = PirType(RType::str).scalar();
                        break;
                    }

                    static const std::unordered_set<std::string> vecTests = {
                        "is.na", "is.nan", "is.finite", "is.infinite"};
                    if (vecTests.count(name)) {
                        if (!getType(c->callArg(0).val()).maybeObj()) {
                            inferred = PirType(RType::logical);
                            if (getType(c->callArg(0).val()).isScalar())
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
                        "is.language", "is.function",  "is.single",
                        "all",         "any"};
                    if (tests.count(name)) {
                        if (!getType(c->callArg(0).val()).maybeObj())
                            inferred =
                                PirType(RType::logical).scalar().notNAOrNaN();
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
#define V(instr) case Tag::instr:
                    VECTOR_RW_INSTRUCTIONS(V);
                    {
                        inferred = i->inferType(getType);

                        // These return primitive values, unless overwritten by
                        // objects
                        if (!i->arg(0).val()->type.maybeObj())
                            inferred = inferred & PirType::val();

                        if (auto e = Extract1_1D::Cast(i)) {
                            if (!inferred.isScalar() &&
                                getType(e->vec()).isA(PirType::num()) &&
                                // named arguments produce named result
                                !getType(e->vec()).maybeHasAttrs() &&
                                getType(e->idx()).isScalar()) {
                                auto range = rangeAnalysis.before(e).range;
                                if (range.count(e->idx())) {
                                    if (range.at(e->idx()).first > 0) {
                                        // Negative numbers as indices make the
                                        // extract return a vector. Only
                                        // positive are safe.
                                        inferred.setScalar();
                                    }
                                }
                            }
                        }
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

    Visitor::run(code->entry, [&](Instruction* i) {
        if (!i->producesRirResult())
            return;
        if (types.count(i))
            i->type = types.at(i);
    });

    return false;
}

} // namespace pir
} // namespace rir
