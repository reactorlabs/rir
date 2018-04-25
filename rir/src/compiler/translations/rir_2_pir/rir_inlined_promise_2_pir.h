#ifndef RIR_INLINDED_PROMISE_2_PIR_H
#define RIR_INLINDED_PROMISE_2_PIR_H

#include "../../util/builder.h"
#include "rir_2_pir.h"

namespace rir {
namespace pir {

class RirInlinedPromise2Rir : public Rir2Pir {
  public:
    RirInlinedPromise2Rir(RirCompiler& cmp, std::string name) : Rir2Pir(cmp, name) { }
  private:
    void compileReturn(pir::Value*) override;
};
} // namespace pir
} // namespace rir
#endif
