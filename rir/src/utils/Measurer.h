#pragma once

#include "R/r_incl.h"
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

// Labels a table
struct MeasureHeader {
    std::string name;
    std::string first;
    std::string second;

    // cppcheck-suppress passedByValue
    MeasureHeader(std::string name, std::string first, std::string second)
        : name(name), first(first), second(second) {}
};

// Keeps track of one type of measurement for one closure or version
struct MeasureRow {
    std::string name;
    unsigned first = 0;
    unsigned second = 0;

    // cppcheck-suppress passedByValue
    explicit MeasureRow(std::string name) : name(name) {}
};

// Keeps track of one type of measurement
struct MeasureTable {
    // Indexed by closure code start, assumed not to be reused. If this is
    // wrong, replace by UUID for each closure
    const std::string fileBase;
    const MeasureHeader header;

    // cppcheck-suppress passedByValue
    MeasureTable(std::string fileBase, MeasureHeader header)
        : fileBase(fileBase), header(header) {}

    MeasureRow& row(pir::ClosureVersion* code, bool create);
    MeasureRow& row(Code* code, SEXP ast, bool create);
    void reset();

    void flush() const;
    void writeCsv(std::ostream& out) const;

  private:
    std::unordered_map<void*, MeasureRow> rows;
};

// Keeps track of all measurements
struct MeasureData {
    MeasureData(MeasureFlags flags, std::string fileBase);

    bool hasTable(MeasureFlag flag) const;
    MeasureTable& table(MeasureFlag flag);
    void reset();

  private:
    std::unordered_map<MeasureFlag, MeasureTable> tables;
};

// Keeps track and records all measurements
struct Measurer {
    MeasureData data;

    // cppcheck-suppress passedByValue
    explicit Measurer(MeasureData data) : data(data) {}

    void recordClosureStart(Code* code, SEXP ast, bool isInline);
    void recordClosureMkEnv(Code* code, bool beforeStart = false,
                            SEXP ast = NULL);
    void recordCompiled(pir::ClosureVersion* code);
    void recordOptimized(pir::ClosureVersion* code);
};

}; // namespace rir
