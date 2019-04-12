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
    {
        std::unordered_map<size_t, std::vector<size_t>> classes;
        std::unordered_map<Value*, size_t> number;
        std::unordered_map<SEXP, size_t> constants;

        bool changed = true;
        size_t nextNumber = 0;

        // Adhoc phi numbering to account for recursive phis
        // exploits commutative nature of inputs
        // TODO: is there a more standard way??
        auto computePhiNr = [&](Phi* phi, bool& success) -> size_t {
            SmallSet<BB*> seen;
            success = true;
            std::function<size_t(Phi*)> doComputePhiNr =
                [&](Phi* phi) -> size_t {
                if (!success || seen.includes(phi->bb()))
                    return 0;
                seen.insert(phi->bb());

                std::set<size_t> inputs;
                phi->eachArg([&](BB* bb, Value* a) {
                    if (number.count(a))
                        inputs.insert(hash_combine(number.at(a), bb));
                    else if (auto phi = Phi::Cast(a))
                        inputs.insert(hash_combine(doComputePhiNr(phi), bb));
                    else
                        success = false;
                });

                auto h = phi->gvnBase();
                for (auto n : inputs)
                    h = hash_combine(h, n);
                return h;
            };
            auto res = doComputePhiNr(phi);
            return res;
        };

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
                return nextNumber;
            };

            auto i = Instruction::Cast(v);
            if (!i) {
                assignNumber(v);
                return;
            }

            if (!gvnEffects.empty()) {
                assignNumber(i);
                return;
            }

            if (!i->producesRirResult())
                return;

            auto computeNumber = [&](Instruction* i) {
                size_t klassNumber = i->gvnBase();

                std::vector<size_t> args;
                if (auto phi = Phi::Cast(i)) {
                    bool success = false;
                    auto res = computePhiNr(phi, success);
                    if (success) {
                        changed = true;
                        number[phi] = res;
                        if (!firstValue.count(res))
                            firstValue[res] = phi;
                    }
                    return;
                }

                bool success = true;
                i->eachArg([&](Value* a) {
                    if (success && number.count(a))
                        klassNumber = hash_combine(klassNumber, number.at(a));
                    else
                        success = false;
                });
                if (success) {
                    i->eachArg([&](Value* a) { args.push_back(number.at(a)); });

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
                constants.emplace(constant, nextNumber);
                storeNumber(i, nextNumber, {nextNumber});
                return;
            }

            computeNumber(i);
        };

        while (changed) {
            changed = false;
            Visitor::run(cls->entry, [&](BB* bb) {
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
        DominanceGraph dom(cls);

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
                    i->clearEffects();
                    replacements[i] = first;
                }
            }
        }
    }
}

} // namespace pir
} // namespace rir
