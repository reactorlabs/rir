//
// Created by Jakob Hain on 6/6/23.
//

#include "SerialModule.h"
#include "R/Serialize.h"
#include "compiler/native/SerialRepr.h"
#include "compiler/native/pir_jit_llvm.h"
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_os_ostream.h>

namespace rir {

static llvm::ExitOnError ExitOnErr;

SerialModule::SerialModule(const llvm::Module& module) {
    llvm::raw_string_ostream os(bitcode);
    // In the future, if we want deterministic and hashable modules (e.g. want
    // to share between compiler servers), we will set
    // ShouldPreserveUseListOrder and GenerateHash to true
    llvm::WriteBitcodeToFile(module, os);
    os.flush();
}

std::unique_ptr<llvm::Module> SerialModule::decode() const {
    llvm::StringRef data(bitcode);
    llvm::MemoryBufferRef buffer(data, "rir::SerialModule");
    auto mod = ExitOnErr(llvm::parseBitcodeFile(buffer, pir::PirJitLLVM::getContext()));
    pir::SerialRepr::patch(*mod);
    return mod;
}

SerialModule SerialModule::deserialize(R_inpstream_t inp) {
    size_t size = InInteger(inp);
    std::string bitcode(size, '\0');
    InBytes(inp, (uint8_t*)bitcode.data(), (int)size);
    return SerialModule(std::move(bitcode));
}

void SerialModule::serialize(R_outpstream_t out) const {
    OutInteger(out, (int)bitcode.size());
    OutBytes(out, (const uint8_t*)bitcode.data(), (int)bitcode.size());
}

std::ostream& operator<<(std::ostream& out, const SerialModule& m) {
    auto mod = m.decode();
    llvm::raw_os_ostream ro(out);
    mod->print(ro, nullptr, true, true);
    return out;
}

} // namespace rir