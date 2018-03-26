#include "pir_translator.h"

namespace rir {
    bool PirTranslator::getVerbose() { return verbose;}
    void PirTranslator::setVerbose(bool isVerbose) { verbose = isVerbose; }
    pir::Module* PirTranslator::getModule(){return module; }
}    
