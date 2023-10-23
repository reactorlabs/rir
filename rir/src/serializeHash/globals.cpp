//
// Created by Jakob Hain on 8/13/23.
//

#include "globals.h"
#include "R/Symbols.h"

namespace rir {

std::vector<SEXP>* globals_;
std::unordered_set<SEXP>* globalsSet_;
std::unordered_map<SEXP, unsigned>* global2Index_;
std::unordered_map<std::string, SEXP>* cppId2Global_;
std::unordered_map<SEXP, std::string>* global2CppId_;
const std::vector<SEXP>& globals = *globals_;
const std::unordered_set<SEXP>& globalsSet = *globalsSet_;
const std::unordered_map<SEXP, unsigned>& global2Index = *global2Index_;
const std::unordered_map<std::string, SEXP>& cppId2Global = *cppId2Global_;
const std::unordered_map<SEXP, std::string>& global2CppId = *global2CppId_;

void initGlobals() {
    cppId2Global_ = new std::unordered_map<std::string, SEXP>();
    cppId2Global_->emplace("R_GlobalEnv", R_GlobalEnv);
    cppId2Global_->emplace("R_BaseEnv", R_BaseEnv);
    cppId2Global_->emplace("R_BaseNamespace", R_BaseNamespace);
    cppId2Global_->emplace("R_TrueValue", R_TrueValue);
    cppId2Global_->emplace("R_NilValue", R_NilValue);
    cppId2Global_->emplace("R_FalseValue", R_FalseValue);
    cppId2Global_->emplace("R_UnboundValue", R_UnboundValue);
    cppId2Global_->emplace("R_MissingArg", R_MissingArg);
    cppId2Global_->emplace("R_RestartToken", R_RestartToken);
    cppId2Global_->emplace("R_LogicalNAValue", R_LogicalNAValue);
    cppId2Global_->emplace("R_EmptyEnv", R_EmptyEnv);
    cppId2Global_->emplace("R_DimSymbol", R_DimSymbol);
    cppId2Global_->emplace("R_DotsSymbol", R_DotsSymbol);
    cppId2Global_->emplace("R_NamesSymbol", R_NamesSymbol);
    cppId2Global_->emplace("expandDotsTrigger", symbol::expandDotsTrigger);
    cppId2Global_->emplace("closureEnvStub", symbol::closureEnvStub);

    globals_ = new std::vector<SEXP>();
    globalsSet_ = new std::unordered_set<SEXP>();
    global2CppId_ = new std::unordered_map<SEXP, std::string>();
    for (auto& e : *cppId2Global_) {
        globals_->push_back(e.second);
        globalsSet_->insert(e.second);
        global2CppId_->emplace(e.second, e.first);
    }
    global2Index_ = new std::unordered_map<SEXP, unsigned>();
    for (unsigned i = 0; i < globals_->size(); ++i) {
        global2Index_->emplace((*globals_)[i], i);
    }
}


} // namespace rir