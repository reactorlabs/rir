//
// Created by Jakob Hain on 7/28/23.
//

#pragma once

#include "R/r_incl.h"
#include "runtime/log/RirObjectPrintStyle.h"
#include <ostream>
#include <functional>

namespace rir {

typedef const std::function<void(std::ostream&)>& PrettyGraphContentPrinter;

class PrettyGraphInnerPrinter {
    std::function<void(PrettyGraphContentPrinter name)> addName_;
    std::function<void(PrettyGraphContentPrinter body)> addBody_;
    std::function<void(SEXP connected, bool isChild, const char* type, PrettyGraphContentPrinter description, bool isFarAway)> addEdgeTo_;
    PrettyGraphInnerPrinter(
        const std::function<void(PrettyGraphContentPrinter name)>&& addName,
        const std::function<void(PrettyGraphContentPrinter body)>&& addBody,
        const std::function<void(SEXP connected, bool isChild, const char* type, PrettyGraphContentPrinter description, bool isFarAway)>&& addEdgeTo)
        : addName_(addName), addBody_(addBody), addEdgeTo_(addEdgeTo) {}

  public:
    /// Given a function which prints an SEXP's node content and adds connected
    /// nodes via the inner printer, this function prints an HTML graph
    /// containing the node and all its connected nodes. i.e. this function
    /// maintains the SEXP worklist and prints the header and footer, as well
    /// as constructing an PrettyGraphInnerPrinter which lets you print the
    /// content.
    ///
    /// This should generally not be called. It's used by
    /// `printRirObject(,,RirObjectPrintStyle::PrettyGraph)` which you probably
    /// want to use instead.
    static void
    printUsingImpl(SEXP root, std::ostream& out,
                   std::function<void(SEXP sexp, const PrettyGraphInnerPrinter& print)> printImpl);

    void addName(PrettyGraphContentPrinter name) const {
        addName_(name);
    }

    void addBody(PrettyGraphContentPrinter body) const {
        addBody_(body);
    }

    void addEdgeTo(SEXP connected, bool isChild, const char* type,
                   PrettyGraphContentPrinter description = [](std::ostream& s){},
                   bool isFarAway = false) const {
        addEdgeTo_(connected, isChild, type, description, isFarAway);
    }
};

} // namespace rir
