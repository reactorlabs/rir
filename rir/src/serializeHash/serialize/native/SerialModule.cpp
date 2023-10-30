//
// Created by Jakob Hain on 6/6/23.
//

#include "SerialModule.h"
#include "R/Serialize.h"
#include "SerialRepr.h"
#include "compiler/native/pir_jit_llvm.h"
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_os_ostream.h>

namespace rir {

static llvm::ExitOnError ExitOnErr;

SerialModule::SerialModule(size_t bitcodeSize, const SerialOptions& serialOpts) // NOLINT(*-pass-by-value)
    : RirRuntimeObject(0, 0), serialOpts(serialOpts),
      bitcodeSize(bitcodeSize) {}

SerialModule::SerialModule(std::string&& bitcode, const SerialOptions& serialOpts) // NOLINT(*-pass-by-value)
    : SerialModule(bitcode.size(), serialOpts) {
    std::copy(bitcode.begin(), bitcode.end(), this->bitcode);
}

SEXP SerialModule::create(std::string&& bitcode, const SerialOptions& serialOpts) {
    auto store = Rf_allocVector(EXTERNALSXP, (R_xlen_t)size(bitcode.size()));
    new (DATAPTR(store)) SerialModule(std::move(bitcode), serialOpts);
    return store;
}

size_t SerialModule::size(size_t bitcodeSize) {
    return sizeof(SerialModule) + bitcodeSize;
}

SEXP SerialModule::create(const llvm::Module& module,
                          const SerialOptions& serialOpts) {
    std::string bitcode;
    llvm::raw_string_ostream os(bitcode);
    // In the future, if we want deterministic and hashable modules (e.g. want
    // to share between compiler servers), we will set
    // ShouldPreserveUseListOrder and GenerateHash to true
    llvm::WriteBitcodeToFile(module, os);
    os.flush();

    return create(std::move(bitcode), serialOpts);
}

std::unique_ptr<llvm::Module>
SerialModule::decode(Code* outer,
                     const SerialOptions& overrideSerialOpts) const {
    assert(serialOpts.areCompatibleWith(overrideSerialOpts) &&
           "serial options module is decoded with must be compatible with "
           "those it was encoded with");
    llvm::StringRef data(bitcode, bitcodeSize);
    llvm::MemoryBufferRef buffer(data, "rir::SerialModule");
    auto mod = ExitOnErr(llvm::parseBitcodeFile(buffer, pir::PirJitLLVM::getContext()));
    pir::SerialRepr::patch(*mod, outer, overrideSerialOpts);
    return mod;
}

std::unique_ptr<llvm::Module> SerialModule::decode(Code* outer) const {
    return decode(outer, serialOpts);
}

size_t SerialModule::size() const {
    return size(bitcodeSize);
}

uint64_t SerialModule::firstBitcodeBytes() const {
    uint64_t result = 0;
    for (size_t i = 0; i < std::min((size_t)8, bitcodeSize); ++i) {
        result |= ((uint64_t)bitcode[i]) << (i * 8);
    }
    return result;
}

void SerialModule::print(std::ostream& out) const {
    auto mod = decode(nullptr);
    llvm::raw_os_ostream ro(out);
    mod->print(ro, nullptr, true, true);
}

SerialModule* SerialModule::deserialize(AbstractDeserializer& deserializer) {
    auto serialOpts = SerialOptions::deserializeCompatible(deserializer, SerialFlags::CodeNative);

    auto bitcodeSize = deserializer.readBytesOf<size_t>(SerialFlags::CodeNative);
    auto store = Rf_allocVector(EXTERNALSXP, size(bitcodeSize));
    auto module = new (DATAPTR(store)) SerialModule(bitcodeSize, serialOpts);
    // Magic is already set. Also, SerialModule isn't actually recursive, we
    // just use refs because we don't want copies.
    deserializer.addRef(store);

    deserializer.readBytes((void*)module->bitcode, bitcodeSize, SerialFlags::CodeNative);

    return unpack(store);
}

void SerialModule::serialize(AbstractSerializer& serializer) const {
    serialOpts.serializeCompatible(serializer, SerialFlags::CodeNative);

    serializer.writeBytesOf(bitcodeSize, SerialFlags::CodeNative);
    serializer.writeBytes((const void*)bitcode, bitcodeSize,
                          SerialFlags::CodeNative);
}

void SerialModule::hash(HasherOld& hasher) const {
    serialOpts.hashCompatible(hasher);

    hasher.hashBytesOf(bitcodeSize);
    hasher.hashBytes(bitcode, bitcodeSize);
}

void SerialModule::addConnected(__attribute__((unused))
                                ConnectedCollectorOld& collector) const {
    // No connected UUIDs
}

} // namespace rir