#include "Measure.h"
#ifdef ENABLE_MEASURE
#include "compiler/analysis/query.h"
#include "compiler/pir/pir_impl.h"
#include "utils/capture_out.h"
#include <fstream>
#include <sstream>
#include <unistd.h>

namespace rir {

void MeasureTable::record(Opcode op, Code* code) {
    assert(op != Opcode::invalid_);
    sumRow.counts[(unsigned)op]++;
}

void MeasureTable::reset() {
    sumRow.reset();
}

void MeasureTable::flush() const {
    if (!filePath.empty()) {
        bool header = access(filePath.c_str(), F_OK) == -1;
        std::ofstream out(filePath.c_str(), std::ios_base::app);
        writeCsv(header, out);
    }
}

void MeasureTable::writeCsv(bool header, std::ostream& out) const {
    // Padding
    unsigned pads[MeasureRow::ncols];
    pads[0] = 30;
    int i = 1;
#define DEF_INSTR(name, ...)                                                   \
    pads[i] = strlen(#name);                                                   \
    i++;
#include "../ir/insns.h"
#undef DEF_INSTR
    (void)i;

    if (header) {
        size_t lastSlash = filePath.find_last_of("/\\");
        std::string fileName = filePath.substr(lastSlash + 1);
        out << std::left << std::setw(pads[0]) << fileName;
#define DEF_INSTR(name, ...) out << ", " << #name;
#include "../ir/insns.h"
#undef DEF_INSTR
        out << "\n";
    }

    out << std::left << std::setw(pads[0]) << title;
    i = 0;
#define DEF_INSTR(name, ...)                                                   \
    out << ", " << std::right << std::setw(pads[i + 1]) << sumRow.counts[i];   \
    i++;
#include "../ir/insns.h"
#undef DEF_INSTR
    out << "\n";
    (void)i;

    out.flush();
}

}; // namespace rir
#endif
