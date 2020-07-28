#include "compiler/pir/pir_impl.h"
#include "compiler/util/cfg.h"
#include "compiler/util/visitor.h"
#include "pass_definitions.h"
#include "utils/Set.h"

namespace rir {
namespace pir {

bool Overflow::apply(RirCompiler&, ClosureVersion* cls, Code* code,
                     LogStream&) const {
    UsesTree uses(code);

    auto willDefinitelyNotOverflow = [&](Instruction* instr) {
        assert(Add::Cast(instr) || Sub::Cast(instr));
        std::unordered_set<const Instruction*> seen;
        Instruction::ArgumentValuePredicateIterator isWithSimpleForIndex =
            [&](const Value* value) {
                value = value->cFollowCasts();
                auto isUsedByColonCastRhs = [&]() {
                    if (auto instruction =
                            Instruction::Cast(const_cast<Value*>(value))) {
                        for (auto use : uses.at(instruction)) {
                            if (ColonCastRhs::Cast(use)) {
                                return true;
                            }
                        }
                    }
                    return false;
                };
                auto isComparedToColonCastRhs = [&]() {
                    if (auto instruction =
                            Instruction::Cast(const_cast<Value*>(value))) {
                        for (auto use : uses.at(instruction)) {
                            if (Neq::Cast(use) && use->anyArg([&](Value* arg) {
                                    return ColonCastRhs::Cast(arg);
                                })) {
                                return true;
                            }
                        }
                    }
                    return false;
                };
                if (ColonCastLhs::Cast(value) || isUsedByColonCastRhs() ||
                    isComparedToColonCastRhs()) {
                    return true;
                } else if (Add::Cast(value) || Sub::Cast(value) ||
                           Phi::Cast(value)) {
                    const Instruction* instr = (const Instruction*)value;
                    if (!seen.count(instr)) {
                        seen.insert(instr);
                        return instr->anyArg(isWithSimpleForIndex);
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            };
        return isWithSimpleForIndex(instr);
    };

    // binops on non-NA integers will produce NA iff they overflow.
    // So normally binops on non-NA typed integers produce a maybe-NA typed
    // integer. Here, we find these binop instructions, check if they won't
    // overflow / underflow, and if so refine the result type to non-NA.
    Visitor::run(code->entry, [&](Instruction* instr) {
        if (!Add::Cast(instr) && !Sub::Cast(instr))
            return;
        // is a binop which we can infer may not overflow / underflow
        if (!instr->allNonEnvArgs([&](Value* arg) {
                return arg->type.maybe(RType::integer) &&
                       (!arg->type.maybeNAOrNaN() ||
                        (Phi::Cast(arg) &&
                         ((Instruction*)arg)->allNonEnvArgs([&](Value* phiArg) {
                             return !phiArg->type.maybeNAOrNaN() ||
                                    phiArg == instr;
                         })));
            }))
            return;
        // is on non-NA typed integers
        if (!instr->type.maybeNAOrNaN())
            return;
        // didn't already infer that it's non-NA
        if (!willDefinitelyNotOverflow(instr))
            return;
        // will definitely not overflow / underflow
        // so we set the result type to non-NA
        instr->type = instr->type.notNAOrNaN();
    });

    return false;
}

} // namespace pir
} // namespace rir
