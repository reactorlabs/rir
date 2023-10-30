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

SerialModule::SerialModule(const llvm::Module& module,
                           const SerialOptions& serialOpts) // NOLINT(*-pass-by-value)
    : bitcode(), serialOpts(serialOpts) {
    llvm::raw_string_ostream os(bitcode);
    // In the future, if we want deterministic and hashable modules (e.g. want
    // to share between compiler servers), we will set
    // ShouldPreserveUseListOrder and GenerateHash to true
    llvm::WriteBitcodeToFile(module, os);
    os.flush();
}

std::unique_ptr<llvm::Module>
SerialModule::decode(Code* outer,
                     const SerialOptions& overrideSerialOpts) const {
    assert(serialOpts.areCompatibleWith(overrideSerialOpts) &&
           "serial options module is decoded with must be compatible with "
           "those it was encoded with");
    llvm::StringRef data(bitcode);
    llvm::MemoryBufferRef buffer(data, "rir::SerialModule");
    auto mod = ExitOnErr(llvm::parseBitcodeFile(buffer, pir::PirJitLLVM::getContext()));
    pir::SerialRepr::patch(*mod, outer, overrideSerialOpts);
    return mod;
}

std::unique_ptr<llvm::Module> SerialModule::decode(Code* outer) const {
    return decode(outer, serialOpts);
}

SerialModule SerialModule::deserializeR(R_inpstream_t inp) {
    ByteBuffer serialOptsBuf((size_t)InInteger(inp));
    InBytes(inp, serialOptsBuf.data(), (int)serialOptsBuf.size());
    SerialOptions serialOpts = SerialOptions::deserializeCompatible(serialOptsBuf);

    size_t size = InInteger(inp);
    std::string bitcode(size, '\0');
    InBytes(inp, (uint8_t*)bitcode.data(), (int)size);

    return {std::move(bitcode), serialOpts};
}

void SerialModule::serializeR(R_outpstream_t out) const {
    ByteBuffer serialOptsBuf;
    serialOpts.serializeCompatible(serialOptsBuf);
    OutInteger(out, (int)serialOptsBuf.size());
    OutBytes(out, serialOptsBuf.data(), (int)serialOptsBuf.size());

    OutInteger(out, (int)bitcode.size());
    OutBytes(out, (const uint8_t*)bitcode.data(), (int)bitcode.size());
}

SerialModule SerialModule::deserialize(AbstractDeserializer& deserializer) {
    auto serialOpts = SerialOptions::deserializeCompatible(deserializer);

    auto size = deserializer.readBytesOf<size_t>();
    std::string bitcode(size, '\0');
    deserializer.readBytes((void*)bitcode.data(), size);

    return {std::move(bitcode), serialOpts};
}

void SerialModule::serialize(AbstractSerializer& serializer) const {
    serialOpts.serializeCompatible(serializer);

    serializer.writeBytesOf(bitcode.size());
    serializer.writeBytes((const void*)bitcode.data(), bitcode.size());
}

size_t SerialModule::numBytes() const {
    return sizeof(size_t) + bitcode.size();
}

std::ostream& operator<<(std::ostream& out, const SerialModule& m) {
    auto mod = m.decode(nullptr);
    llvm::raw_os_ostream ro(out);
    mod->print(ro, nullptr, true, true);
    return out;
}

} // namespace rir