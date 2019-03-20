#include "../pir/pir.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void GVN::apply(RirCompiler&, ClosureVersion* cls, LogStream& log) const {

    std::unordered_map<size_t, SmallSet<Value*>> reverseNumber;
    std::unordered_map<size_t, Value*> firstValue;
    {
        std::unordered_map<size_t, SmallSet<size_t>> classes;
        std::unordered_map<Value*, size_t> number;
        std::unordered_map<SEXP, size_t> constants;

        bool changed = true;
        size_t nextNumber = 0;

        std::function<void(Value*)> computeGN = [&](Value* v) -> void {
            if (number.count(v))
                return;

            auto storeNumber = [&](Value* v, size_t n, SmallSet<size_t> klass) {
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

            auto computeNumber = [&](Instruction* i) {
                size_t klassNumber = (size_t)i->tag;
                bool success = true;
                i->eachArg([&](Value* a) {
                    if (success && number.count(a))
                        klassNumber = hash_combine(klassNumber, number.at(a));
                    else
                        success = false;
                });

                if (success) {
                    SmallSet<size_t> klass;
                    i->eachArg([&](Value* a) { klass.insert(number.at(a)); });

                    if (!classes.count(klassNumber) ||
                        classes.at(klassNumber) != klass) {
                        while (classes.count(klassNumber))
                            klassNumber++;
                        storeNumber(i, klassNumber, klass);
                    } else {
                        number[i] = klassNumber;
                    }
                    changed = true;
                }
            };

            switch (i->tag) {
            case Tag::LdConst: {
                SEXP constant = LdConst::Cast(i)->c();
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
                break;
            }
            case Tag::Mul:
            case Tag::Div:
            case Tag::IDiv:
            case Tag::Mod:
            case Tag::Add:
            case Tag::Colon:
            case Tag::Pow:
            case Tag::Sub:
            case Tag::Gte:
            case Tag::Lte:
            case Tag::Gt:
            case Tag::Lt:
            case Tag::Neq:
            case Tag::Eq:
            case Tag::LAnd:
            case Tag::LOr:
            case Tag::Not:
            case Tag::Plus:
            case Tag::Minus:
            case Tag::Length:
            case Tag::AsTest:
            case Tag::Identical:
            case Tag::IsObject: {
                if (i->effects.includes(Effect::ExecuteCode))
                    assignNumber(i);
                else
                    computeNumber(i);
                break;
            }
            case Tag::Phi:
                computeNumber(i);
                break;
            default:
                assignNumber(i);
                break;
            };
        };

        while (changed) {
            changed = false;
            BreadthFirstVisitor::run(cls->entry,
                                     [&](Instruction* i) { computeGN(i); });
        }

        for (auto& g : number)
            reverseNumber[g.second].insert(g.first);

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
        DominanceGraph dom(cls);

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
