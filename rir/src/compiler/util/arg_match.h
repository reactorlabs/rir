#ifndef PIR_ARGUMENT_MATCHER_H
#define PIR_ARGUMENT_MATCHER_H

#include "../pir/pir.h"
#include "R/RList.h"
#include "compiler/pir/builder.h"
#include "ir/BC_inc.h"

namespace rir {
namespace pir {

struct ArgumentMatcher {
    static bool reorder(Builder& insert, SEXP formals,
                        const std::vector<BC::PoolIdx>& actualNames,
                        std::vector<Value*>& given);
};

} // namespace pir
} // namespace rir

#endif
