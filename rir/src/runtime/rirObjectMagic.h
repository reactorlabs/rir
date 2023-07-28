//
// Created by Jakob Hain on 7/26/23.
//

#pragma once

#include "R/r_incl.h"

namespace rir {

/// Throws an error if the object isn't an EXTERNALSXP (RIR object)
unsigned rirObjectMagic(SEXP rirObject);

/// Class name of rir object with the given magic
const char* rirObjectClassName(unsigned magic);

/// Class name of rir object.
/// Throws an error if the object isn't an EXTERNALSXP (RIR object)
const char* rirObjectClassName(SEXP rirObject);

} // namespace rir
