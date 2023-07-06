//
// Created by Jakob Hain on 6/24/23.
//

#pragma once

#include "R/r_incl.h"
#include "bc/BC.h"
#include "runtime/Deoptimization.h"

namespace llvm {
class Module;
class LLVMContext;
class MDNode;
}

namespace rir {
namespace pir {

class SerialRepr {
  protected:
    explicit SerialRepr() {}

  public:
    static constexpr const char* POINTER_METADATA_NAME = "rir.serial.pointer";
    static constexpr const char* FUNCTION_METADATA_NAME = "rir.serial.function";
    static constexpr const char* SRC_IDX_METADATA_NAME = "rir.serial.srcIdx";
    static constexpr const char* POOL_IDX_METADATA_NAME = "rir.serial.poolIdx";
    static constexpr const char* NAMES_METADATA_NAME = "rir.serial.names";

    class SEXP;
    class String;
    class Code;
    class DeoptMetadata;
    class OpaqueTrue;
    class R_Visible;
    class R_BCNodeStackTop;
    class R_GlobalContext;
    class R_ReturnedValue;

    virtual llvm::MDNode* metadata(llvm::LLVMContext& ctx) const = 0;
    static llvm::MDNode* functionMetadata(llvm::LLVMContext& ctx,
                                          const char* llvmValueName,
                                          int builtinId);
    static llvm::MDNode* srcIdxMetadata(llvm::LLVMContext& ctx,
                                        Immediate srcIdx);
    static llvm::MDNode* poolIdxMetadata(llvm::LLVMContext& ctx,
                                         BC::PoolIdx poolIdx);
    static llvm::MDNode* namesMetadata(llvm::LLVMContext& ctx,
                                       const std::vector<BC::PoolIdx>& names);

    /// Replace pointers with the serialized encodings, fetching from the
    /// compiler server if necessary. See lower_function_llvm.cpp for where
    /// exactly we store the metadata
    static void patch(llvm::Module& mod);
};

class SerialRepr::SEXP : public SerialRepr {
    ::SEXP what;

  public:
    explicit SEXP(::SEXP what) : SerialRepr(), what(what) {}

    llvm::MDNode* metadata(llvm::LLVMContext& ctx) const override;
};
class SerialRepr::String : public SerialRepr {
    const char* str;

  public:
    explicit String(const char* str) : SerialRepr(), str(str) {}

    llvm::MDNode* metadata(llvm::LLVMContext& ctx) const override;
};
class SerialRepr::Code : public SerialRepr {
    rir::Code* code;

  public:
    Code(rir::Code* code) : SerialRepr(), code(code) {}

    llvm::MDNode* metadata(llvm::LLVMContext& ctx) const override;
};
class SerialRepr::DeoptMetadata : public SerialRepr {
    rir::DeoptMetadata* m;

  public:
    explicit DeoptMetadata(rir::DeoptMetadata* m) : SerialRepr(), m(m) {}

    llvm::MDNode* metadata(llvm::LLVMContext& ctx) const override;
};
class SerialRepr::OpaqueTrue : public SerialRepr {
  public:
    OpaqueTrue() : SerialRepr() {}

    llvm::MDNode* metadata(llvm::LLVMContext& ctx) const override;
};
class SerialRepr::R_Visible : public SerialRepr {
  public:
    R_Visible() : SerialRepr() {}

    llvm::MDNode* metadata(llvm::LLVMContext& ctx) const override;
};
class SerialRepr::R_BCNodeStackTop : public SerialRepr {
  public:
    R_BCNodeStackTop() : SerialRepr() {}

    llvm::MDNode* metadata(llvm::LLVMContext& ctx) const override;
};
class SerialRepr::R_GlobalContext : public SerialRepr {
  public:
    R_GlobalContext() : SerialRepr() {}

    llvm::MDNode* metadata(llvm::LLVMContext& ctx) const override;
};
class SerialRepr::R_ReturnedValue : public SerialRepr {
  public:
    R_ReturnedValue() : SerialRepr() {}

    llvm::MDNode* metadata(llvm::LLVMContext& ctx) const override;
};

} // namespace pir
} // namespace rir
