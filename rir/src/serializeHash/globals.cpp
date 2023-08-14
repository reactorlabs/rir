//
// Created by Jakob Hain on 8/13/23.
//

#include "globals.h"
#include "R/Symbols.h"

namespace rir {

std::vector<SEXP> globals;
std::unordered_map<SEXP, unsigned> global2Index;
std::unordered_map<std::string, SEXP> cppId2Global;
std::unordered_map<SEXP, std::string> global2CppId;

void initGlobals() {
    cppId2Global = std::unordered_map<std::string, SEXP>();
    cppId2Global.emplace("R_GlobalEnv", R_GlobalEnv);
    cppId2Global.emplace("R_BaseEnv", R_BaseEnv);
    cppId2Global.emplace("R_BaseNamespace", R_BaseNamespace);
    cppId2Global.emplace("R_TrueValue", R_TrueValue);
    cppId2Global.emplace("R_NilValue", R_NilValue);
    cppId2Global.emplace("R_FalseValue", R_FalseValue);
    cppId2Global.emplace("R_UnboundValue", R_UnboundValue);
    cppId2Global.emplace("R_MissingArg", R_MissingArg);
    cppId2Global.emplace("R_RestartToken", R_RestartToken);
    cppId2Global.emplace("R_LogicalNAValue", R_LogicalNAValue);
    cppId2Global.emplace("R_EmptyEnv", R_EmptyEnv);
    cppId2Global.emplace("R_DimSymbol", R_DimSymbol);
    cppId2Global.emplace("R_DotsSymbol", R_DotsSymbol);
    cppId2Global.emplace("R_NamesSymbol", R_NamesSymbol);
    cppId2Global.emplace("expandDotsTrigger", symbol::expandDotsTrigger);

    globals = std::vector<SEXP>();
    global2CppId = std::unordered_map<SEXP, std::string>();
    for (auto& e : cppId2Global) {
        globals.push_back(e.second);
        global2CppId.emplace(e.second, e.first);
    }
    global2Index = std::unordered_map<SEXP, unsigned>();
    for (unsigned i = 0; i < globals.size(); ++i) {
        global2Index.emplace(globals[i], i);
    }
}


} // namespace rir