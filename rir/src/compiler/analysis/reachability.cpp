#include "reachability.h"
#include "available_checkpoints.h"
#include "cfg.h"

namespace rir {
namespace pir {

Reachability::Reachability(ClosureVersion* cls, Code* code, LogStream& log)
    : cfg(*new CFG(code)), cp(*new AvailableCheckpoints(cls, code, log)),
      allocated(true) {}

Reachability::Reachability(const CFG& cfg, const AvailableCheckpoints& cp)
    : cfg(cfg), cp(cp), allocated(false) {}

Reachability::~Reachability() {
    if (allocated) {
        delete &cfg;
        delete &cp;
    }
}

bool Reachability::operator()(Instruction* a, BB* b) const {
    if (a->bb() == b)
        return true;
    if (cfg.isPredecessor(a->bb(), b))
        return true;
    if (auto c = cp.at(a))
        if (cfg.isPredecessor(c->bb(), b))
            return true;
    return false;
}

} // namespace pir
} // namespace rir
