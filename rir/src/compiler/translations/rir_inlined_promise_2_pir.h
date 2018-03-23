#ifndef RIR_INLINDED_PROMISE_2_PIR_H
#define RIR_INLINDED_PROMISE_2_PIR_H

#include "../util/builder.h"
#include "rir_2_pir.h"

namespace rir {

class RirInlinedPromise2Rir : public Rir2Pir {
  public:
    RirInlinedPromise2Rir(pir::Builder* builder)
        : Rir2Pir(builder) {}

  private:
    void addReturn(pir::Value*);
};
}
#endif