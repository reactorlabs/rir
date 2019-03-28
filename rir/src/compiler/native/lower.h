#ifndef PIR_NATIVE_LOWER_H
#define PIR_NATIVE_LOWER_H

#include "compiler/pir/pir.h"
#include "runtime/Code.h"

#include "jit/jit-plus.h"
#include <unordered_map>
#include <vector>

namespace rir {
namespace pir {

class Lower {
  public:
    void* tryCompile(Code* code, const std::unordered_map<Promise*, unsigned>&);
};
}
}

#endif
