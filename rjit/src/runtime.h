#ifndef RUNTIME_H_
#define RUNTIME_H_

#include <inttypes.h>

namespace rjit {

const uint64_t patchpointSize = 10;

extern uint64_t nextStackmapId;

} // namespace rjit

#endif // RUNTIME_H_
