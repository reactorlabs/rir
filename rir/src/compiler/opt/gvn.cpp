#include "../pir/pir.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"
#include <set>

namespace rir {
namespace pir {

void GVN::apply(RirCompiler&, ClosureVersion* cls, LogStream& log) const {

    std::unordered_map<size_t, SmallSet<Value*>> reverseNumber;
    std::unordered_map<size_t, Value*> firstValue;
    DominanceGraph dom(cls);
    {
        std::unordered_map<size_t, std::vector<size_t>> classes;
        std::unordered_map<Value*, size_t> number;
        std::unordered_map<SEXP, size_t> constants;

        bool changed = true;
        size_t nextNumber = 0;

        std::function<void(Value*)> computeGN = [&](Value* v) -> void {
            if (number.count(v))
                return;

            auto storeNumber = [&](Value* v, size_t n,
                                   std::vector<size_t> klass) {
                assert(!number.count(v));
                assert(!firstValue.count(n));
                number[v] = n;
                classes[n] = klass;
                firstValue[n] = v;
                changed = true;
            };

            auto assignNumber = [&](Value* v) {
                while (classes.count(nextNumber))
                    nextNumber++;
                storeNumber(v, nextNumber, {nextNumber});
            };

            auto i = Instruction::Cast(v);
            if (!i)
                return assignNumber(v);

            if (!i->producesRirResult())
                return;

            auto computeNumber = [&](Instruction* i) {
                size_t klassNumber = i->gvnBase();

                bool success = true;
                std::vector<size_t> args;
                if (auto phi = Phi::Cast(i)) {
                    // Special case phi: instruction is commutative, but we
                    // additionally need to associate input block ids with
                    // input values to avoid confusing (BB1:a, BB2:b) with
                    // (BB2:a, BB1:b).
                    klassNumber = hash_combine(klassNumber, phi->bb()->id);
                    std::set<size_t> sortedArgs;
                    phi->eachArg([&](BB* bb, Value* a) {
                        if (success) {
                            if (number.count(a))
                                sortedArgs.insert(
                                    hash_combine(number.at(a), bb->id));
                            else if (auto p = Phi::Cast(a))
                                sortedArgs.insert(
                                    hash_combine(p->gvnBase(), bb->id));
                            else
                                success = false;
                        }
                    });
                    if (success) {
                        for (auto& n : sortedArgs) {
                            klassNumber = hash_combine(klassNumber, n);
                            args.push_back(n);
                        }
                    }
                } else {
                    i->eachArg([&](Value* a) {
                        if (success && number.count(a))
                            klassNumber =
                                hash_combine(klassNumber, number.at(a));
                        else
                            success = false;
                    });
                    if (success)
                        i->eachArg(
                            [&](Value* a) { args.push_back(number.at(a)); });
                }

                if (success) {
                    if (!classes.count(klassNumber) ||
                        classes.at(klassNumber) != args) {
                        while (classes.count(klassNumber))
                            klassNumber++;
                        storeNumber(i, klassNumber, args);
                    } else {
                        number[i] = klassNumber;
                    }
                    changed = true;
                }
            };

            if (auto ld = LdConst::Cast(i)) {
                SEXP constant = ld->c();
                for (auto& c : constants) {
                    if (R_compute_identical(c.first, constant, 0)) {
                        number[i] = c.second;
                        changed = true;
                        return;
                    }
                }
                while (classes.count(nextNumber))
                    nextNumber++;
                constants[constant] = nextNumber;
                storeNumber(i, nextNumber, {nextNumber});
            } else {
                // We allow instructions with those effects to be deduplicated.
                auto effects =
                    i->effects & (~(Effects(Effect::Error) | Effect::Warn |
                                    Effect::Visibility | Effect::Force));
                if (effects.empty())
                    computeNumber(i);
                else
                    assignNumber(i);
            }
        };

        while (changed) {
            changed = false;
            DominatorTreeVisitor<>(dom).run(cls->entry, [&](BB* bb) {
                for (auto i : *bb)
                    computeGN(i);
            });
        }

        for (auto& g : number) {
            reverseNumber[g.second].insert(g.first);
        }

        for (auto it = reverseNumber.begin(); it != reverseNumber.end();) {
            if (it->second.size() < 2)
                it = reverseNumber.erase(it);
            else
                it++;
        }
        if (reverseNumber.size() == 0)
            return;
    }

    {
        std::unordered_map<Value*, Value*> replacements;

        for (auto& g : reverseNumber) {
            auto p = g.second.begin();
            auto first = firstValue.at(g.first);
            while (replacements.count(first))
                first = replacements.at(first);
            auto firstInstr = Instruction::Cast(first);

            while (p != g.second.end()) {
                auto i = Instruction::Cast(*p);
                p++;
                if (i && i != first) {
                    if (firstInstr) {
                        if (i->bb() == firstInstr->bb()) {
                            if (!i->bb()->before(firstInstr, i))
                                continue;
                        } else {
                            if (!dom.dominates(firstInstr->bb(), i->bb()))
                                continue;
                        }
                    }
                    i->replaceUsesWith(first);
                    // Make sure this instruction really gets removed
                    i->effects.reset();
                    replacements[i] = first;
                }
            }
        }
    }
}

} // namespace pir
} // namespace rir
