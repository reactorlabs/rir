//
// Created by Jakob Hain on 7/26/23.
//

#include "printRirObject.h"
#include "R/Printing.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include <sstream>

namespace rir {

void printRirObject(SEXP sexp, std::ostream& s, RirObjectPrintStyle style) {
    if (auto d = DispatchTable::check(sexp)) {
        d->print(s, RirObjectPrintStyle::Detailed);
    } else if (auto f = Function::check(sexp)) {
        f->print(s, RirObjectPrintStyle::Detailed);
    } else if (auto c = Code::check(sexp)) {
        c->print(s, RirObjectPrintStyle::Detailed);
    } else {
        s << Print::dumpSexp(sexp, SIZE_MAX) << "\n";
    }
}

std::string printRirObject(SEXP sexp, RirObjectPrintStyle style) {
    std::stringstream s;
    printRirObject(sexp, s, style);
    return s.str();
}

} // namespace rir