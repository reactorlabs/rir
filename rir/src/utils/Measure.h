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
    static const unsigned ncols = (unsigned)Opcode::num_of + 2;

    std::string name;
    std::array<size_t, (unsigned)Opcode::num_of> counts;

    // cppcheck-suppress passedByValue
    explicit MeasureRow(std::string name) : name(name) { reset(); }

    void reset() { counts.fill(0); }

    size_t total() const {
        size_t res = 0;
        for (size_t c : counts)
            res += c;
        return res;
    }
};

// Keeps track of bytecode invocations for all closures in one run
struct MeasureTable {
    // Indexed by closure code start, assumed not to be reused. If this is
    // wrong, replace by UUID for each closure
    const std::string title;
    const std::string fileName;

    // cppcheck-suppress passedByValue
    MeasureTable(std::string title)
        : title(title), fileName(title + ".csv"), sumRow(MeasureRow("Total")) {}

    void record(Opcode op, Code* code);
    void reset();

    void flush() const;
    void writeCsv(std::ostream& out) const;
    bool isEnabled() const { return title != ""; }

  private:
    std::unordered_map<void*, MeasureRow> rows;
    MeasureRow sumRow;

    MeasureRow& row(Code* code);
};

}; // namespace rir
#endif
