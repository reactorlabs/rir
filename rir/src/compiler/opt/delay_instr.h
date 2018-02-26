#ifndef PIR_DELAY_INSTR_H
#define PIR_DELAY_INSTR_H

namespace rir {
namespace pir {

class Function;
class DelayInstr {
  public:
    static void apply(Function*);
};
}
}

#endif
