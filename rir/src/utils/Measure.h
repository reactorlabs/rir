#pragma once
#ifdef ENABLE_MEASURE

#include "R/r_incl.h"
#include "ir/BC_inc.h"
#include "utils/EnumSet.h"

#include <cstdint>
#include <iostream>
#include <math.h>
#include <unordered_map>

namespace rir {

struct Code;
struct InterpreterInstance;

namespace pir {
class ClosureVersion;
};

// Keeps track of bytecode invocations for one closure in one run
struct MeasureRow {
    static const unsigned ncols = (unsigned)Opcode::num_of + 1;

    std::array<size_t, (unsigned)Opcode::num_of> counts;

    void reset() { counts.fill(0); }
};

// Keeps track of bytecode invocations for all closures in one run
struct MeasureTable {
    // Indexed by closure code start, assumed not to be reused. If this is
    // wrong, replace by UUID for each closure
    const std::string title;
    const std::string filePath;

    // cppcheck-suppress passedByValue
    explicit MeasureTable(std::string title, std::string filePath)
        : title(title), filePath(filePath), sumRow(MeasureRow()) {}

    void record(Opcode op, Code* code);
    void reset();

    void flush() const;
    void writeCsv(bool header, std::ostream& out) const;
    bool isEnabled() const { return title != ""; }

  private:
    MeasureRow sumRow;
};

}; // namespace rir
#endif
