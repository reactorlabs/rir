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

                PirType infered = PirType::bottom();
                switch (i->tag) {
                case Tag::Mul:
                case Tag::Div:
                case Tag::IDiv:
                case Tag::Mod:
                case Tag::Add:
                case Tag::Pow:
                case Tag::Sub:
                case Tag::Not:
                case Tag::Plus:
                case Tag::Minus:
                case Tag::Phi: {
                    i->eachArg([&](Value* v) {
                        if (i->mayHaveEnv() && v == i->env())
                            return;
                        infered = infered | getType(v);
                    });
                    if (i->tag == Tag::Div && infered.isA(RType::integer)) {
                        infered = infered | RType::real;
                    }
                    break;
                }
                case Tag::CallSafeBuiltin: {
                    auto c = CallSafeBuiltin::Cast(i);
                    std::string name = getBuiltinName(getBuiltinNr(c->blt));
                    if ("bitwiseXor" == name || "bitwiseShiftL" == name) {
                        infered = PirType(RType::integer);
                        if (getType(c->arg(0).val()).isScalar() &&
                            getType(c->arg(1).val()).isScalar())
                            infered.setScalar();
                        break;
                    }
                }
                // fall through
                default:
                    infered = i->type;
                }

                if (!types.count(i) || types.at(i) != infered) {
                    done = false;
                    types[i] = infered;
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
