//
// Created by Jakob Hain on 6/24/23.
//

#pragma once

#include "R/r_incl.h"
#include "runtime/Deoptimization.h"

namespace llvm {
class LLVMContext;
class MDNode;
}

namespace rir {
namespace pir {

class SerialRepr {
  protected:
    explicit SerialRepr() {}

  public:
    class SEXP;
    class String;
    class DeoptMetadata;
    class OpaqueTrue;
    class R_Visible;
    class R_BCNodeStackTop;
    class R_GlobalContext;

    virtual llvm::MDNode* metadata(llvm::LLVMContext& ctx) const = 0;
    static llvm::MDNode* functionMetadata(llvm::LLVMContext& ctx,
                                          const char* llvmValueName,
                                          int builtinId);
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

} // namespace pir
} // namespace rir
