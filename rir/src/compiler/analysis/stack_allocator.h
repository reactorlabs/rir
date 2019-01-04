#pragma once

#include "../analysis/generic_static_analysis.h"
#include "../pir/pir.h"
#include "stack_values.h"

namespace rir {
namespace pir {

/*
 *
 */
class StackAllocator {
  public:
    using Compensation = std::vector<std::pair<rir::Opcode, uint32_t>>;
    using ApplyCompensation = std::unordered_map<Instruction*, Compensation>;
    using MergeCompensation =
        std::unordered_map<BB*, std::unordered_map<BB*, Compensation>>;

    ApplyCompensation applyCompensation;
    MergeCompensation mergeCompensation;

    StackAllocator(Closure* fun, Code* code, LogStream& log);
};

} // namespace pir
} // namespace rir
