#ifndef INSTRUMENTATION_H
#define INSTRUMENTATION_H

#include "RDefs.h"
#include "TypeInfo.h"

#include <llvm/IR/Function.h>

namespace rjit {

class TypeRecorder {
  public:
    TypeRecorder(SEXP store);
    void record(SEXP value, int idx);

  private:
    SEXP store;
};

class TypeFeedback {
  public:
    TypeFeedback(SEXP native);

    void clearInvocationCount();
    TypeInfo get(SEXP symbol);

    void attach(llvm::Function* f);
    static TypeFeedback* get(llvm::Function* f);
    static void detach(llvm::Function* f);

  private:
    SEXP cp();
    SEXP native;
};
}

extern "C" void recordType(SEXP value, SEXP store, int idx);

#endif
