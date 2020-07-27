#include "R/r.h"
#include "compiler/pir/pir.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/bb_transform.h"
#include "compiler/util/visitor.h"
#include "pass_definitions.h"
#include "utils/Map.h"
#include <set>

namespace rir {
namespace pir {

bool GVN::apply(Compiler&, ClosureVersion* cls, Code* code,
                LogStream& log) const {
    std::unordered_map<size_t, SmallSet<Value*>> reverseNumber;
    std::unordered_map<Value*, size_t> number;
    {
        std::unordered_map<size_t, std::vector<size_t>> classes;
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
                assert(!seen.includes(phi->bb()));

                if (!success)
                    return 0;
                seen.insert(phi->bb());

                std::set<size_t> inputs;
                phi->eachArg([&](BB* bb, Value* a) {
                    if (a == phi) {
                        inputs.insert(bb->id);
                    } else if (number.count(a)) {
                        inputs.insert(hash_combine(number.at(a), bb));
                    } else if (auto phi = Phi::Cast(a)) {
                        if (!seen.count(phi->bb()))
                            inputs.insert(hash_combine(doComputePhiNr(phi), bb));
                    } else {
                        success = false;
                    }
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
                number[v] = n;
                classes[n] = klass;
                changed = true;
            };

            auto assignNumber = [&](Value* v) {
                while (classes.count(nextNumber))
                    nextNumber++;
                storeNumber(v, nextNumber, {nextNumber});
                return nextNumber;
            };

            if (v->type.isVoid())
                return;

            auto i = Instruction::Cast(v);
            if (!i) {
                assignNumber(v);
                return;
            }

            if (i->gvnBase() == 0) {
                assignNumber(i);
                return;
            }

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

            if (auto phi = Phi::Cast(i)) {
                bool success = false;
                auto res = computePhiNr(phi, success);
                if (success) {
                    changed = true;
                    number[phi] = res;
                }
                return;
            }

            size_t klassNumber = i->gvnBase();

            std::vector<size_t> args;

            bool success = true;
            i->eachArg([&](Value* a) {
                if (!success)
                    return;
                if (!Instruction::Cast(a))
                    computeGN(a);
                if (number.count(a))
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

        while (changed) {
            changed = false;
            Visitor::run(code->entry, [&](BB* bb) {
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
            return false;
    }

    {
        DominanceGraph dom(code);

        typedef std::set<std::pair<size_t, size_t>> PhiClass;
        auto computePhiClass = [&](Phi* phi, PhiClass& res) -> bool {
            bool success = true;
            phi->eachArg([&](BB* bb, Value* a) {
                if (a == phi)
                    res.insert({0, bb->id});
                else if (number.count(a))
                    res.insert({number.at(a), bb->id});
                else
                    success = true;
            });
            return success;
        };

        SmallMap<Phi*, PhiClass> phiClassCache;
        for (auto& g : reverseNumber) {
            auto p = g.second.begin();

            Instruction* firstInstr = nullptr;
            for (auto can : g.second) {
                if (auto cani = Instruction::Cast(can)) {
                    if (!firstInstr ||
                        dom.strictlyDominates(cani->bb(), firstInstr->bb()))
                        firstInstr = cani;
                }
            }
            if (!firstInstr)
                continue;

            while (p != g.second.end()) {
                auto i = Instruction::Cast(*p);
                p++;
                if (i && i != firstInstr) {
                    if (!firstInstr->type.isA(i->type))
                        continue;
                    if (i->bb() == firstInstr->bb()) {
                        if (!i->bb()->before(firstInstr, i))
                            continue;
                    } else if (!dom.dominates(firstInstr->bb(), i->bb())) {
                        continue;
                    }

                    // Phi GNV numbers might clash and actual classes are
                    // checked lazily.
                    if (auto p1 = Phi::Cast(i)) {
                        if (auto p2 = Phi::Cast(firstInstr)) {

                            auto p1ci = phiClassCache.find(p1);
                            if (p1ci == phiClassCache.end()) {
                                PhiClass cl;
                                if (computePhiClass(p1, cl)) {
                                    p1ci = phiClassCache.insert(p1, cl);
                                } else {
                                    continue;
                                }
                            }
                            auto p1c = p1ci->second;

                            auto p2ci = phiClassCache.find(p2);
                            if (p2ci == phiClassCache.end()) {
                                PhiClass cl;
                                if (computePhiClass(p2, cl)) {
                                    p2ci = phiClassCache.insert(p2, cl);
                                } else {
                                    continue;
                                }
                            }
                            auto p2c = p2ci->second;

                            if (p1c.size() != p2c.size() ||
                                !std::equal(p1c.begin(), p1c.end(),
                                            p2c.begin()))
                                continue;
                        } else {
                            continue;
                        }
                    }
                    i->replaceUsesWith(firstInstr);
                    // Make sure this instruction really gets removed
                    i->effects.reset();
                }
            }
        }
    }

    // Remove dead instructions here, instead of deferring to the cleanup pass.
    // Sometimes a dead instruction will trip the verifier.
    BBTransform::removeDeadInstrs(code, 1);

    // The current implementation of GVN almost always finds something to
    // change. We use the changed flag to determine when to stop optimizing and
    // it is thus just too noisy.
    return false;
}

} // namespace pir
} // namespace rir
