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
extern std::vector<SEXP> globals;
extern std::unordered_map<std::string, SEXP> cppId2Global;
extern std::unordered_map<SEXP, std::string> global2CppId;

/// Initialize globals. Needs to run after symbols are initialized
void initGlobals();

} // namespace rir
