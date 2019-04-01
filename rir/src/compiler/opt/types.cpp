#include "../pir/pir_impl.h"
#include "../translations/pir_translator.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

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
                        if (auto arg = Instruction::Cast(v)) {
                            if (types.count(arg))
                                infered = infered | types.at(arg);
                            else
                                done = false;
                        } else {
                            infered = infered | v->type;
                        }
                    });
                    if (i->tag == Tag::Div && infered.isA(RType::integer)) {
                        infered = infered | RType::real;
                    }
                    break;
                }
                case Tag::Assume: {
                    auto assumption = Assume::Cast(i);
                    if (!assumption->assumeTrue)
                        if (auto isO = IsObject::Cast(assumption->condition()))
                            if (auto val =
                                    Instruction::Cast(isO->arg<0>().val())) {
                                if (types.count(val))
                                    infered = types.at(val).notObject();
                                else
                                    infered = val->type.notObject();
                            }
                    break;
                }
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
