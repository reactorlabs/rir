#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

bool CleanupCheckpoints::apply(RirCompiler&, ClosureVersion* cls, Code* code,
                               LogStream&) const {
    bool anyChange = false;
        std::unordered_set<Checkpoint*> used;
        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto a = Assume::Cast(i)) {
                used.insert(a->checkpoint());
            }
        });

        std::unordered_set<BB*> toDelete;
        Visitor::run(code->entry, [&](BB* bb) {
            if (bb->isEmpty())
                return;
            if (auto cp = Checkpoint::Cast(bb->last())) {
                if (!used.count(cp)) {
                    toDelete.insert(bb->deoptBranch());
                    assert(bb->deoptBranch()->isExit() &&
                           "deopt blocks should be just one BB");
                    bb->remove(bb->end() - 1);
                    bb->convertBranchToJmp(true);
                }
            }
        });
        if (!toDelete.empty())
            anyChange = true;
        // Deopt blocks are exit blocks. They have no other predecessors and
        // are not phi inputs. We can delete without further checks.
        for (auto bb : toDelete)
            delete bb;
    return anyChange;
}
} // namespace pir
} // namespace rir
