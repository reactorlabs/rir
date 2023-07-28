//
// Created by Jakob Hain on 7/26/23.
//

#include "printRirObject.h"
#include "R/Printing.h"
#include "printPrettyGraph.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include <sstream>

namespace rir {

static void defaultPrintRirObject(SEXP sexp, std::ostream& s, RirObjectPrintStyle& style) {
    switch (style) {
    case RirObjectPrintStyle::Default:
        s << Print::dumpSexp(sexp) << "\n";
        break;
    case RirObjectPrintStyle::Detailed:
        s << Print::dumpSexp(sexp, SIZE_MAX) << "\n";
        break;
    case RirObjectPrintStyle::PrettyGraph:
    case RirObjectPrintStyle::PrettyGraphInner:
        printPrettyGraph(sexp, s, style, [&](PrettyGraphInnerPrinter print){
            print.addBody([&](std::ostream& s){ s << Print::dumpSexp(sexp); });
        });
    }
}

void printRirObject(SEXP sexp, std::ostream& s, RirObjectPrintStyle style) {
    if (auto d = DispatchTable::check(sexp)) {
        d->print(s, style);
    } else if (auto f = Function::check(sexp)) {
        f->print(s, style);
    } else if (auto c = Code::check(sexp)) {
        c->print(s, style);
    } else {
        defaultPrintRirObject(sexp, s, style);
    }
}

std::string printRirObject(SEXP sexp, RirObjectPrintStyle style) {
    std::stringstream s;
    printRirObject(sexp, s, style);
    return s.str();
}

} // namespace rir