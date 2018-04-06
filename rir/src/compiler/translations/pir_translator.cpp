#include "pir_translator.h"
#include "rir_compiler.h"

namespace rir {
namespace pir {

void PirTranslator::apply(Closure* function) { 
    this->applyTranslation(function);
}
}
}    
