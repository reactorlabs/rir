//
// Created by Jakob Hain on 7/28/23.
//

#pragma once

#include "R/r_incl.h"
#include "runtime/log/RirObjectPrintStyle.h"
#include <ostream>

namespace rir {

using PrettyGraphContentPrinter = const std::function<void(std::ostream&)>&;

class PrettyGraphInnerPrinter {
    friend void
    printPrettyGraph(SEXP sexp, std::ostream& s, RirObjectPrintStyle style,
                     const std::function<void(PrettyGraphInnerPrinter)>& printInner);

    const std::function<void(PrettyGraphContentPrinter name)>& addName_;
    const std::function<void(PrettyGraphContentPrinter body)>& addBody_;
    const std::function<void(SEXP connected, bool isChild, const char* type, PrettyGraphContentPrinter description, bool isFarAway)>& addEdgeTo_;
    PrettyGraphInnerPrinter(
        const std::function<void(PrettyGraphContentPrinter name)>& addName,
        const std::function<void(PrettyGraphContentPrinter body)>& addBody,
        const std::function<void(SEXP connected, bool isChild, const char* type, PrettyGraphContentPrinter description, bool isFarAway)>& addEdgeTo)
        : addName_(addName), addBody_(addBody), addEdgeTo_(addEdgeTo) {}

  public:
    void addName(PrettyGraphContentPrinter name) {
        addName_(name);
    }

    void addBody(PrettyGraphContentPrinter body) {
        addBody_(body);
    }

    void addEdgeTo(SEXP connected, bool isChild, const char* type,
                   PrettyGraphContentPrinter description,
                   bool isFarAway = false) {
        addEdgeTo_(connected, isChild, type, description, isFarAway);
    }
};

void printPrettyGraph(SEXP sexp, std::ostream& s, RirObjectPrintStyle style,
                      const std::function<void(PrettyGraphInnerPrinter)>& printInner);

} // namespace rir
