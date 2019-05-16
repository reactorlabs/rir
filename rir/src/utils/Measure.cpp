#include "Measure.h"
#ifdef ENABLE_MEASURE
#include "compiler/analysis/query.h"
#include "compiler/pir/pir_impl.h"
#include "utils/capture_out.h"
#include <fstream>
#include <sstream>
#include <unistd.h>

namespace rir {

MeasureRow& MeasureTable::row(Code* code) {
    void* key = code->code();
    if (!rows.count(key)) {
        std::string str;
        {
            CaptureOut rec;
            Rf_PrintValue(code->ast());
            str = rec.oneline(30);
        }
        MeasureRow row(str);
        rows.emplace(key, row);
    }
    return rows.at(key);
}

void MeasureTable::record(Opcode op, Code* code) {
    assert(op != Opcode::invalid_);
    row(code).counts[(unsigned)op]++;
    sumRow.counts[(unsigned)op]++;
}

void MeasureTable::reset() {
    rows.clear();
    sumRow.reset();
}

void MeasureTable::flush() const {
    if (!fileName.empty()) {
        std::ofstream out(fileName.c_str(), std::ios_base::trunc);
        writeCsv(out);
    }
}

void MeasureTable::writeCsv(std::ostream& out) const {
    // Padding
    unsigned pads[MeasureRow::ncols];
    pads[0] = title.size();
    for (auto& entry : rows) {
        const MeasureRow& row = entry.second;
        pads[0] = std::max(pads[0], (unsigned)row.name.size());
    }
    pads[0] = std::max(pads[0], (unsigned)sumRow.name.size());
    int i = 1;
#define DEF_INSTR(name, ...)                                                   \
    pads[i] = strlen(#name);                                                   \
    i++;
#include "../ir/insns.h"
#undef DEF_INSTR
    pads[i] = strlen("Total");

    // Header
    out << std::left << std::setw(pads[0]) << title;
#define DEF_INSTR(name, ...) out << ", " << #name;
#include "../ir/insns.h"
#undef DEF_INSTR
    out << ", Total\n";

    // Rows
    for (auto& entry : rows) {
        const MeasureRow& row = entry.second;
        out << std::left << std::setw(pads[0]) << row.name;
        i = 1;
#define DEF_INSTR(name, ...)                                                   \
    out << ", " << std::right << std::setw(pads[i]) << row.counts[i];          \
    i++;
#include "../ir/insns.h"
#undef DEF_INSTR
        out << ", " << std::right << std::setw(pads[i]) << row.total() << "\n";
    }
    out.flush();
}

}; // namespace rir
#endif
