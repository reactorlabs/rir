#ifndef RIR_VECOPR_
#define RIR_VECOPR_

#include <iostream>
#include <string>
#include <unistd.h>
#include <R/Protect.h>
#include "api.h"
class VecOpr {
  public:

  static SEXP insertElementAndGrow(SEXP vec, SEXP ele) {
    assert(TYPEOF(vec) == VECSXP);
    rir::Protect p;
    int oldSize = Rf_length(vec);
    SEXP newVec;
    p(newVec = Rf_allocVector(VECSXP, oldSize + 1));
    memcpy(DATAPTR(newVec), DATAPTR(vec), oldSize * sizeof(SEXP));
    SET_VECTOR_ELT(newVec, oldSize, ele);
    return newVec;
  }

  static SEXP insertUniqueSymbolElementAndGrow(SEXP vec, SEXP ele) {
    assert(TYPEOF(vec) == VECSXP && TYPEOF(ele) == SYMSXP);
    // Existing keys are no added
    for (int i = 0; i < Rf_length(vec); i++) {
      SEXP e = VECTOR_ELT(vec, i);
      if (e == ele) {
        return vec;
      }
    }
    rir::Protect p;
    int oldSize = Rf_length(vec);
    SEXP newVec;
    p(newVec = Rf_allocVector(VECSXP, oldSize + 1));
    memcpy(DATAPTR(newVec), DATAPTR(vec), oldSize * sizeof(SEXP));
    SET_VECTOR_ELT(newVec, oldSize, ele);
    return newVec;
  }

  static void replace(SEXP vec, int index, SEXP val) {
    assert(TYPEOF(vec) == VECSXP);
    SET_VECTOR_ELT(vec, index, val);
  }

  static SEXP get(SEXP vec, int index) {
    assert(TYPEOF(vec) == VECSXP);
    return VECTOR_ELT(vec, index);
  }

  static void remove(SEXP vec, int index) {
    assert(TYPEOF(vec) == VECSXP);
    SET_VECTOR_ELT(vec, index, R_NilValue);
  }

  static void remove(SEXP vec, SEXP val) {
    assert(TYPEOF(vec) == VECSXP);
    for (int i = 0; i < Rf_length(vec); i++) {
      SEXP ele = VECTOR_ELT(vec, i);
      if (ele == val) {
        SET_VECTOR_ELT(vec, i, R_NilValue);
      }
    }
  }

  static void print(SEXP vec) {
    assert(TYPEOF(vec) == VECSXP);
    std::cout << "printing Vector: " << std::endl;
    for (int i = 0; i < Rf_length(vec); i++) {
      SEXP ele = VECTOR_ELT(vec, i);
      std::cout << "   vector[" << i << "]" << std::endl;
      printAST(3,ele);
    }
  }
  private:
};

#endif
