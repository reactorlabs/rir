#ifndef PIR_2_RIR_H
#define PIR_2_RIR_H

#include "pir_translator.h"

namespace rir {

class Pir2Rir : public PirTranslator {
  public:
    pir::Function* compileFunction(SEXP);
    pir::Function* compileFunction(pir::IRTransformation*);
};

}

#endif
