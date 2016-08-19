#ifndef RIR_PRESERVE_H
#define RIR_PRESERVE_H

#include "R/r.h"
#include <vector>

namespace rir {

class Preserve {
  public:
    Preserve(const Preserve& other) = delete;

    Preserve(){};

    SEXP operator()(SEXP value) {
        R_PreserveObject(value);
        p.push_back(value);
        return value;
    }

    ~Preserve() {
        for (auto o : p)
            R_ReleaseObject(o);
    }

  private:
    std::vector<SEXP> p;
};
}

#endif
