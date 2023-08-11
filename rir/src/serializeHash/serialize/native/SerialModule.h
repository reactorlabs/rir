//
// Created by Jakob Hain on 6/6/23.
//

#pragma once

#include "R/r_incl.h"
#include "serializeHash/serializeUni.h"
#include <memory>
#include <string>

namespace llvm {

class Module;

} // namespace llvm

namespace rir {

struct Code;
class SerialModule;
/// Serialized module bitcode. We store these in smart pointers these because
/// multiple `Code`s may share the same module.
///
/// We also intern these because the `Code`s are deserialized so we can't always
/// determine and give them the same shared_ptr at creation. But [PirJitLLVM] is
/// where we intern.
typedef std::shared_ptr<SerialModule> SerialModuleRef;

namespace pir {
class PirJitLLVM;
}

/// Serialized module bitcode
class SerialModule {
    std::string bitcode;

    explicit SerialModule(std::string&& bitcode) : bitcode(std::move(bitcode)) {}

    // These methods WOULD be public, except we don't want to accidentally call
    // them without PirJitLLVM because the modules won't actually be added to
    // LLJit and currently we always want to add them to LLJIT.
    friend class pir::PirJitLLVM;
    explicit SerialModule(const llvm::Module& module);
    std::unique_ptr<llvm::Module> decode(Code* outer) const;
    static SerialModule deserializeR(R_inpstream_t inp);
    static SerialModule deserialize(AbstractDeserializer& deserializer);
  public:
    void serializeR(R_outpstream_t out) const;
    void serialize(AbstractSerializer& serializer) const;
    friend std::ostream& operator<<(std::ostream&, const SerialModule&);
};

} // namespace rir
