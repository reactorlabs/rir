#ifndef PIR_DELAY_INSTR_H
#define PIR_DELAY_INSTR_H

namespace rir {
namespace pir {

/*
 * DelayInstr tries to schedule instruction right before they are needed.
 *
 */
class Closure;
class DelayInstr {
  public:
    static void apply(Closure*);
};
}
}

#endif
