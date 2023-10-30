//
// Created by Jakob Hain on 6/6/23.
//

#pragma once

#include "R/r_incl.h"
#include "runtime/RirRuntimeObject.h"
#include "serializeHash/hash/getConnectedOld.h"
#include "serializeHash/serialize/serialize.h"
#include "serializeHash/serializeUni.h"
#include <memory>
#include <string>
#include <utility>

namespace llvm {

class Module;

} // namespace llvm

namespace rir {

struct Code;
struct SerialOptions;

namespace pir {
class PirJitLLVM;
}

/// "SMOD" ASCII -> hex
#define SERIAL_MODULE_MAGIC 0x534d4f44

/// Serialized module bitcode
class SerialModule
    : public RirRuntimeObject<SerialModule, SERIAL_MODULE_MAGIC> {
    SerialOptions serialOpts;
    size_t bitcodeSize;
    char bitcode[];

    SerialModule(size_t bitcodeSize, const SerialOptions& serialOpts); // NOLINT(*-pass-by-value)
    SerialModule(std::string&& bitcode, const SerialOptions& serialOpts); // NOLINT(*-pass-by-value)
    static SEXP create(std::string&& bitcode, const SerialOptions& serialOpts);

    /// Size of the `SerialModule` structure from its `bitcodeSize`
    static size_t size(size_t bitcodeSize);

    // These methods WOULD be public, except we don't want to accidentally call
    // them without PirJitLLVM because the modules won't actually be added to
    // LLJit and currently we always want to add them to LLJIT.
    friend class pir::PirJitLLVM;
    static SEXP create(const llvm::Module& module, const SerialOptions& serialOpts);
    std::unique_ptr<llvm::Module> decode(
        Code* outer, const SerialOptions& overrideSerialOpts) const;
    std::unique_ptr<llvm::Module> decode(Code* outer) const;
  public:
    size_t size() const;
    uint64_t firstBitcodeBytes() const;

    void print(std::ostream&) const;
    static SerialModule* deserialize(AbstractDeserializer& deserializer);
    void serialize(AbstractSerializer& serializer) const;
    void hash(HasherOld& hasher) const;
    void addConnected(ConnectedCollectorOld& collector) const;
};

} // namespace rir
