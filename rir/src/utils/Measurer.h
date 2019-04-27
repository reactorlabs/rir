#pragma once

#include "R/r_incl.h"
#include "utils/EnumSet.h"
#include <cstdint>
#include <iostream>
#include <math.h>
#include <unordered_map>

namespace rir {

namespace pir {
class ClosureVersion;
};

// !!! important: when adding a new flag, update MeasureFlag::LAST !!!
#define LIST_OF_RIR_MEASURE_FLAGS(V)                                           \
    V(Envs)                                                                    \
    V(Vars)                                                                    \
    V(LazyArgs)

// A type of measurement
enum class MeasureFlag : uint8_t {
#define V(Flag) Flag,
    LIST_OF_RIR_MEASURE_FLAGS(V)
#undef V

        FIRST = Envs,
    LAST = LazyArgs,
};
typedef EnumSet<MeasureFlag> MeasureFlags;

struct MeasureRow {
    unsigned initial;
    unsigned optimized;

    MeasureRow(unsigned initial) : initial(initial), optimized(initial) {}

    void recordNew(unsigned o) { optimized = o; }
};

// Keeps track of one type of measurements
struct MeasureTable {
    MeasureTable(std::function<unsigned(pir::ClosureVersion*)> measureFunc,
                 std::string fileName);

    void writeCsv(std::ostream& out) const;
    void recordInitial(pir::ClosureVersion* code);
    void recordNew(pir::ClosureVersion* code);
    void reset();

  private:
    const std::function<unsigned(pir::ClosureVersion*)> measureFunc;
    std::unordered_map<std::string, MeasureRow> rows;
    std::string fileName;
    void readCsv(std::istream& in);
    void update();
};

// Keeps track of all measurements
struct Measurer {
    // Doesn't measure anything
    static Measurer empty() { return Measurer(MeasureFlags::None(), ""); }

    Measurer(MeasureFlags flags, std::string fileBase);

    MeasureTable& table(MeasureFlag flag);
    void recordInitial(pir::ClosureVersion* code);
    void recordNew(pir::ClosureVersion* code);
    void reset();

  private:
    std::unordered_map<MeasureFlag, MeasureTable> tables;
};

}; // namespace rir
