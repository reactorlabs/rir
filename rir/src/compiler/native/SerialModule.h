//
// Created by Jakob Hain on 6/6/23.
//

#pragma once

#include <string>
#include "R/r_incl.h"
#include <memory>

namespace llvm {

class Module;

} // namespace llvm

namespace rir {

namespace pir {
class PirJitLLVM;
}

class SerialModule;
/// Serialized module bitcode. We store these in smart pointers these because
/// multiple `Code`s may share the same module.
///
/// We also intern these because the `Code`s are deserialized so we can't always
/// determine and give them the same shared_ptr at creation. But [PirJitLLVM] is
/// where we intern.
typedef std::shared_ptr<SerialModule> SerialModuleRef;

/// Serialized module bitcode
class SerialModule {
    std::string bitcode;

    explicit SerialModule(std::string&& bitcode) : bitcode(std::move(bitcode)) {}

    // These methods WOULD be public, except we don't want to accidentally call
    // them without PirJitLLVM because the modules won't actually be added to
    // LLJit and currently we always want to add them to LLJIT.
    friend class pir::PirJitLLVM;
    explicit SerialModule(const llvm::Module& module);
    std::unique_ptr<llvm::Module> decode() const;
    static SerialModule deserialize(R_inpstream_t inp);
  public:
    void serialize(R_outpstream_t out) const;
    friend std::ostream& operator<<(std::ostream&, const SerialModule&);
};

} // namespace rir
