//
// Created by Jakob Hain on 7/26/23.
//

#pragma once

#include "R/r_incl.h"

namespace rir {

/// Throws an error if the object isn't an EXTERNALSXP (RIR object)
unsigned rirObjectMagic(SEXP rirObject);

} // namespace rir
