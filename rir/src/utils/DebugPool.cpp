#include "DebugPool.h"
#include <vector>

namespace rir {

std::vector<SEXP> DebugPool::tmps;
std::vector<const char*> DebugPool::prefixes;

} // namespace rir
