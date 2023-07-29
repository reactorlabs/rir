//
// Created by Jakob Hain on 7/26/23.
//

#include "printRirObject.h"
#include "R/Printing.h"
#include "printPrettyGraph.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/HTMLBuilder/escapeHtml.h"
#include <sstream>

namespace rir {

static void defaultPrintRirObject(SEXP sexp, std::ostream& s, bool isDetailed) {
    if (isDetailed) {
        s << Print::dumpSexp(sexp, SIZE_MAX) << "\n";
    } else {
        s << Print::dumpSexp(sexp) << "\n";
    }
}

static void
defaultPrintRirObjectPrettyGraphContent(SEXP sexp,
                                        const PrettyGraphInnerPrinter& print) {
    print.addName([&](std::ostream& s){
        s << Rf_type2char(TYPEOF(sexp));
    });
    print.addBody([&](std::ostream& s){
        s << "<pre>" << escapeHtml(Print::dumpSexp(sexp)) << "</pre>";
    });
}

static void printRirObject(SEXP sexp, std::ostream& s, bool isDetailed) {
    if (auto d = DispatchTable::check(sexp)) {
        d->print(s, isDetailed);
    } else if (auto f = Function::check(sexp)) {
        f->print(s, isDetailed);
    } else if (auto c = Code::check(sexp)) {
        c->print(s, isDetailed);
    } else {
        defaultPrintRirObject(sexp, s, isDetailed);
    }
}

static void printRirObjectPrettyGraphContent(SEXP sexp,
                                             const PrettyGraphInnerPrinter& print) {
    if (auto d = DispatchTable::check(sexp)) {
        d->printPrettyGraphContent(print);
    } else if (auto f = Function::check(sexp)) {
        f->printPrettyGraphContent(print);
    } else if (auto c = Code::check(sexp)) {
        c->printPrettyGraphContent(print);
    } else {
        defaultPrintRirObjectPrettyGraphContent(sexp, print);
    }
}

void prettyGraphPrintRirObject(SEXP sexp, std::ostream& s) {
    PrettyGraphInnerPrinter::printUsingImpl(sexp, s, printRirObjectPrettyGraphContent);
}

void printRirObject(SEXP sexp, std::ostream& s, RirObjectPrintStyle style) {
    switch (style) {
    case RirObjectPrintStyle::Default:
        printRirObject(sexp, s, false);
        break;
    case RirObjectPrintStyle::Detailed:
        printRirObject(sexp, s, true);
        break;
    case RirObjectPrintStyle::PrettyGraph:
        prettyGraphPrintRirObject(sexp, s);
        break;
    }
}

std::string printRirObject(SEXP sexp, RirObjectPrintStyle style) {
    std::stringstream s;
    printRirObject(sexp, s, style);
    return s.str();
}

} // namespace rir