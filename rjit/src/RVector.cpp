#include "RIntlns.h"
#include "RVector.h"
#include <cassert>

namespace rjit {

RVector::RVector(size_t init_size) : size_(0), capacity_(slack) {
    vector = Rf_allocVector(VECSXP, init_size);
    SETLENGTH(vector, 0);
    Precious::add(vector);
}

void RVector::append(SEXP e) {
    if (size_ == capacity_) {
        capacity_ *= grow;
        SEXP new_vector = Rf_allocVector(VECSXP, capacity_);
        for (size_t i = 0; i < size_; ++i) {
            SET_VECTOR_ELT(new_vector, i, at(i));
        }
        Precious::remove(vector);
        Precious::add(new_vector);
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
}
