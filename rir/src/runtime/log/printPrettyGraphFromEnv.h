//
// Created by Jakob Hain on 10/10/23.
//

#pragma once

#include "R/r_incl.h"
#include "serializeHash/hash/UUID.h"



namespace rir {

void initializePrintPrettyGraphFromEnv();
void printPrettyGraphOfInternedIfNecessary(SEXP sexp, const UUID& uuid);
void printPrettyGraphOfCompiledIfNecessary(SEXP sexp, const std::string& name);

} // namespace rir
