//
// Created by Jakob Hain on 10/22/23.
//

#pragma once

#include "R/r_incl.h"

namespace rir {

/// "TYPEOF" for special cases, different than any normal SEXP TYPEOF, to ensure
/// they are hashed differently. This is similar to what serialize.c does.
///
/// This has the same size as TYPEOF (unsigned)
enum class SpecialType : SEXPTYPE {
    // Starts at 128, assuming regular SEXPTYPEs only go up to 127, and we
    // remove bytes after 255
    Global = 128,
    Ref = 129,
    Altrep = 130,
    // Only used in writeBc and readBc (when reading and writing bytecode)
    BcRef = 131
};

enum class EnvType {
    Package,
    Namespace,
    Regular
};

unsigned packFlags(SEXPTYPE type, int levs, bool isobj, bool hasattr,
                   bool hastag);
void unpackFlags(unsigned flags, SEXPTYPE& ptype, int& plevs, bool& pisobj,
                 bool& phasattr, bool& phastag);

} // namespace rir
