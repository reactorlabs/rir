#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"
#include "utils/Map.h"
#include "utils/Set.h"

namespace rir {
namespace pir {

bool PromiseSplitter::apply(RirCompiler&, ClosureVersion* cls, Code* code,
                            LogStream&) const {

    bool anyChange = false;
    SmallSet<CastType*> candidates;
    SmallSet<Phi*> banned;
    SmallMap<MkArg*, CastType*> candidateProms;
    SmallMap<CastType*, SmallSet<Instruction*>> uses;

    Visitor::run(code->entry, [&](Instruction* i) {
        if (auto ct = CastType::Cast(i)) {
            if (auto mk = MkArg::Cast(ct->arg(0).val())) {
                candidates.insert(ct);
                candidateProms[mk] = ct;
            }
        }
    });

    Visitor::run(code->entry, [&](Instruction* i) {
        size_t count = 0;
        i->eachArg([&](Value* v) {
            if (auto ct = CastType::Cast(v)) {
                if (candidates.count(ct)) {
                    uses[ct].insert(i);
                    count++;
                }
            }
            if (auto mk = MkArg::Cast(v)) {
                auto ct = candidateProms.find(mk);
                if (ct != candidateProms.end()) {
                    uses[ct->second].insert(i);
                    count++;
                }
            }
        });
        if (count > 1)
            if (auto p = Phi::Cast(i))
                banned.insert(p);
    });

    CFG cfg(code);
    SmallSet<CastType*> toSplit;
    for (const auto& c : candidates) {
        const auto& us = uses.find(c);
        if (us != uses.end()) {
            if (us->second.size() > 1) {
                bool remove = false;
                for (const auto& u1 : us->second) {
                    auto p = Phi::Cast(u1);
                    if (banned.count(p)) {
                        // Phi which uses the promises multiple times. We can't
                        // properly place the copied version in this case
                        remove = true;
                    } else {
                        for (const auto& u2 : us->second)
                            if (u1 != u2 &&
                                (u1->bb() == u2->bb() ||
                                 cfg.isPredecessor(u1->bb(), u2->bb())))
                                remove = true;
                    }
                }
                if (!remove)
                    toSplit.insert(c);
            }
        }
    }

    for (const auto& ct : toSplit) {
        auto mk = MkArg::Cast(ct->arg(0).val());
        for (const auto& u : uses.at(ct)) {
            anyChange = true;
            // copy and insert right before use (or in case of phi, right at the
            // end of the input block)
            auto bb = u->bb();
            auto pos = bb->atPosition(u);
            if (auto phi = Phi::Cast(u)) {
                bool seen = false;
                phi->eachArg([&](BB* inp, Value* v) {
                    if (v == ct || v == mk) {
                        // Can only deal with phi if it uses the promise once
                        assert(!seen);
                        seen = true;
                        bb = inp;
                        pos = inp->end();
                    }
                });
                assert(seen);
            }
            auto nmk = new MkArg(mk->prom(), mk->eagerArg(), mk->env());
            auto nct = new CastType(nmk, ct->kind, ct->arg(0).type(), ct->type);
            pos = bb->insert(pos, nct);
            bb->insert(pos, nmk);
            ct->replaceUsesIn(nct, u->bb());
            mk->replaceUsesIn(nmk, u->bb());
        }
        ct->eraseAndRemove();
        mk->eraseAndRemove();
    }
    return anyChange;
}

} // namespace pir
} // namespace rir
