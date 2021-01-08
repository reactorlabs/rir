#ifndef PIR_REACHABILITY_H
#define PIR_REACHABILITY_H

#include "compiler/pir/pir.h"

namespace rir {
namespace pir {

class CFG;
class AvailableCheckpoints;

class Reachability {
  private:
    const CFG& cfg;
    const AvailableCheckpoints& cp;
    bool allocated;

  public:
    Reachability(ClosureVersion*, Code* code, LogStream& log);
    Reachability(const CFG&, const AvailableCheckpoints&);
    ~Reachability();

    // Checks if basic block b is reachable from right *before* instruction a
    bool operator()(Instruction* a, BB* b) const;
};

} // namespace pir
} // namespace rir
#endif
