#include "pir_translator.h"

namespace rir {
    pir::Module* PirTranslator::compileModule() {
        for (auto f : this->module->functions) {
            this->compileFunction(f->srcFunction);
        }
        return this->module;
    }

    bool PirTranslator::getVerbose() { return verbose;}
    void PirTranslator::setVerbose(bool isVerbose) { verbose = isVerbose; }
    pir::Module* PirTranslator::getModule(){return module; }
}    
