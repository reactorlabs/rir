//
// Created by Jakob Hain on 7/26/23.
//

#pragma once

#include "runtime/log/RirObjectPrintStyle.h"
#include "R/r_incl.h"
#include <iostream>
#include <ostream>

namespace rir {

/// Print an SEXP, printing it detailed if it's RIR
void printRirObject(SEXP sexp, std::ostream& s = std::cout,
                    RirObjectPrintStyle style = RIR_DEBUG_STYLE);
/// Print an SEXP, printing it detailed if it's RIR
std::string printRirObject(SEXP sexp, RirObjectPrintStyle style);

} // namespace rir
