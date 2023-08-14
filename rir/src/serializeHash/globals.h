//
// Created by Jakob Hain on 8/13/23.
//

#pragma once

#include "R/r.h"
#include <string>
#include <vector>
#include <unordered_map>

namespace rir {

// Globals aren't considered connected and references to them don't have
// recursive connected references
extern const std::vector<SEXP>& globals;
extern const std::unordered_map<SEXP, unsigned>& global2Index;
extern const std::unordered_map<std::string, SEXP>& cppId2Global;
extern const std::unordered_map<SEXP, std::string>& global2CppId;

/// Initialize globals. Needs to run after symbols are initialized
void initGlobals();

} // namespace rir
