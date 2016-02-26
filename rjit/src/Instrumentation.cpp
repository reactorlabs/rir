#include "Instrumentation.h"

#include "llvm.h"

#include "RIntlns.h"
#include "TypeInfo.h"
#include "Flags.h"

#include <iostream>
#include <cassert>

namespace rjit {

TypeRecorder::TypeRecorder(SEXP store) : store(store) {
    assert(TYPEOF(store) == INTSXP);
}

void TypeRecorder::record(SEXP value, int idx) {
    assert(idx < XLENGTH(store));

    TypeInfo old_info(INTEGER(store)[idx]);
    TypeInfo info(INTEGER(store)[idx]);

    info.mergeAll(value);

    if (old_info != info) {
        INTEGER(store)[idx] = static_cast<int>(info);
    }
}

TypeFeedback::TypeFeedback(SEXP native) : native(native) {
    assert(TYPEOF(native) == NATIVESXP);
}

SEXP TypeFeedback::cp() { return CDR(native); }

void TypeFeedback::clearInvocationCount() {
    SEXP invocationCount = VECTOR_ELT(cp(), 3);
    INTEGER(invocationCount)[0] = 0;
}

namespace {
static char const* const MD_NAME = "rjit_typefeedback_node";
}

TypeInfo TypeFeedback::get(SEXP sym) {
    SEXP store = VECTOR_ELT(cp(), 1);
    SEXP names = VECTOR_ELT(cp(), 2);
    assert(TYPEOF(store) == INTSXP);
    assert(TYPEOF(names) == VECSXP);
    assert(XLENGTH(store) == XLENGTH(names));
    for (unsigned i = 0; i < XLENGTH(store); ++i) {
        if (VECTOR_ELT(names, i) == sym) {
            return TypeInfo(INTEGER(store)[i]);
        }
    }
    return TypeInfo::any();
}

void TypeFeedback::attach(llvm::Function* f) {
    std::vector<llvm::Metadata*> v = {
        llvm::ValueAsMetadata::get(llvm::ConstantInt::get(
            f->getContext(),
            llvm::APInt(64, reinterpret_cast<std::uintptr_t>(this))))};
    llvm::MDNode* m = llvm::MDNode::get(f->getContext(), v);
    f->setMetadata(MD_NAME, m);
}

TypeFeedback* TypeFeedback::get(llvm::Function* f) {
    llvm::MDNode* m = f->getMetadata(MD_NAME);
    if (m == nullptr)
        return nullptr;
    llvm::Metadata* mx = m->getOperand(0);
    llvm::APInt const& ap =
        llvm::cast<llvm::ConstantInt>(
            llvm::cast<llvm::ValueAsMetadata>(mx)->getValue())
            ->getUniqueInteger();
    assert(ap.isIntN(64) and "Expected 64bit integer");
    TypeFeedback* res = reinterpret_cast<TypeFeedback*>(ap.getZExtValue());
    return res;
}

void TypeFeedback::detach(llvm::Function* f) {
    TypeFeedback* tf = get(f);
    if (tf) {
        f->setMetadata(MD_NAME, nullptr);
        delete tf;
    }
}
}

extern "C" void checkType(SEXP value, rjit::TypeInfo expected) {
    rjit::TypeInfo changed = expected;
    changed.mergeAll(value);
    assert(changed == expected);
}

extern "C" void recordType(SEXP value, SEXP store, int idx) {
    rjit::TypeRecorder record(store);
    record.record(value, idx);
}
