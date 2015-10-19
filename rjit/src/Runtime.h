#ifndef RUNTIME_H_
#define RUNTIME_H_

#include <cinttypes>

#include "RDefs.h"

namespace rjit {

const uint64_t patchpointSize = 12;

} // namespace rjit

extern "C" void patchIC(void* ic, uint64_t stackmapId, void* caller);

extern "C" void* compileIC(uint64_t numargs, SEXP call, SEXP fun, SEXP rho,
                           uint64_t stackmapId);

#endif // RUNTIME_H_
