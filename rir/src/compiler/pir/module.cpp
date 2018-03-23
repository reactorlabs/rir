#include "module.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Module::print(std::ostream& out) {
    for (auto f : functions) {
        f->dstFunction->print(out);
        out << "\n-------------------------------\n";
    }
}

Module::~Module() {
    for (auto f : functions)
        delete f;
}
}
}
