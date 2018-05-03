#ifndef RIR_INLINDED_PROMISE_2_PIR_H
#define RIR_INLINDED_PROMISE_2_PIR_H

#include "../../util/builder.h"
#include "rir_2_pir.h"

namespace rir {
namespace pir {

class RirInlinedPromise2Rir : public Rir2Pir {
  public:
    explicit RirInlinedPromise2Rir(Rir2Pir& functionPir2Rir, rir::Code* promise)
        : Rir2Pir(functionPir2Rir.cmp, functionPir2Rir.insert,
                  functionPir2Rir.srcFunction, promise) {}

  private:
    void compileReturn(pir::Value*) override;
};
} // namespace pir
} // namespace rir
#endif
