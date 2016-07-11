#include "r.h"
#include "RVector.h"
#include "Protect.h"
#include <cassert>

namespace rir {

RVector::RVector(SEXP vector)
    : size_(XLENGTH(vector)), capacity_(XLENGTH(vector)), vector(vector) {
    assert(TYPEOF(vector) == VECSXP);
    R_PreserveObject(vector);
}

SEXP RVectorIter::operator*() { return vector->at(pos); }

RVector::RVector(size_t init_size) : size_(0), capacity_(slack) {
    vector = Rf_allocVector(VECSXP, init_size);
    SETLENGTH(vector, 0);
    R_PreserveObject(vector);
}

void RVector::append(SEXP e) {
    if (size_ == capacity_) {
        Protect p(e);
        capacity_ *= grow;
        SEXP new_vector = Rf_allocVector(VECSXP, capacity_);
        for (size_t i = 0; i < size_; ++i) {
            SET_VECTOR_ELT(new_vector, i, at(i));
        }
        R_ReleaseObject(vector);
        R_PreserveObject(new_vector);
        vector = new_vector;
    }
    size_++;
    SETLENGTH(vector, size_);
    SET_VECTOR_ELT(vector, size_ - 1, e);
}

size_t RVector::insert(SEXP e) {
    for (size_t i = 0; i < size_; ++i) {
        if (at(i) == e)
            return i;
    }
    append(e);
    return size_ - 1;
}

SEXP RVector::at(size_t i) {
    assert(i < size_);
    return VECTOR_ELT(vector, i);
}

size_t RVector::size() { return size_; }

SEXP RVector::operator[](size_t idx) { return VECTOR_ELT(vector, idx); }
}
