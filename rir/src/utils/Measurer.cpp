#include "Measurer.h"
#include "compiler/analysis/query.h"
#include "compiler/pir/pir_impl.h"
#include "utils/capture_out.h"
#include <fstream>
#include <sstream>
#include <unistd.h>

namespace rir {

MeasureRow& MeasureTable::row(pir::ClosureVersion* code, bool create) {
    void* key = code->owner()->rirFunction()->body()->code();
    if (!rows.count(key)) {
        if (!create)
            assert(false);
        MeasureRow row(code->owner()->name());
        rows.emplace(key, row);
    }
    return rows.at(key);
}

MeasureRow& MeasureTable::row(Code* code, bool create) {
    return row(code, code->ast(), create);
}

MeasureRow& MeasureTable::row(Code* code, SEXP ast, bool create) {
    void* key = code->code();
    if (!rows.count(key)) {
        if (!create)
            assert(false);
        std::string str;
        {
            CaptureOut rec;
            Rf_PrintValue(ast);
            str = rec.oneline(30);
        }
        MeasureRow row(str);
        rows.emplace(key, row);
    }
    return rows.at(key);
}

MeasureRow& MeasureTable::row(const char* name, void* key, bool create) {
    if (!rows.count(key)) {
        if (!create)
            assert(false);
        MeasureRow row(name);
        rows.emplace(key, row);
    }
    return rows.at(key);
}

void MeasureTable::reset() {
    rows.clear();
}

void MeasureTable::flush() const {
    if (!fileBase.empty()) {
        std::string fileName = fileBase + std::to_string(getpid()) + ".csv";
        std::ofstream out(fileName.c_str(), std::ios_base::trunc);
        writeCsv(out);
    }
}

void MeasureTable::writeCsv(std::ostream& out) const {
    unsigned pad1 = header.name.size();
    unsigned pad2 = header.first.size();
    unsigned pad3 = header.second.size();
    for (auto& entry : rows) {
        const MeasureRow& row = entry.second;
        pad1 = std::max(pad1, (unsigned)row.name.size());
        pad2 = std::max(pad2, (unsigned)log10((double)row.first));
        pad3 = std::max(pad3, (unsigned)log10((double)row.second));
    }
    out << std::left << std::setw(pad1) << header.name << ", " << std::right
        << std::setw(pad2) << header.first << ", " << std::right
        << std::setw(pad3) << header.second << "\n";
    for (auto& entry : rows) {
        const MeasureRow& row = entry.second;
        out << std::left << std::setw(pad1) << row.name << ", " << std::right
            << std::setw(pad2) << row.first << ", " << std::right
            << std::setw(pad3) << row.second << "\n";
    }
    out.flush();
}

// cppcheck-suppress passedByValue
MeasureData::MeasureData(MeasureFlags flags, std::string fileBase) {
    for (MeasureFlag flag : flags) {
        switch (flag) {
        case MeasureFlag::Envs: {
            MeasureTable table(
                fileBase.empty() ? "" : fileBase + "_envs",
                MeasureHeader("Closure", "Created Envs", "Calls"));
            tables.emplace(flag, table);
            break;
        }
        case MeasureFlag::InferredFuns: {
            MeasureTable table(fileBase.empty() ? "" : fileBase + "_allFuns",
                               MeasureHeader("Caller", "Inferred", "Calls"));
            tables.emplace(flag, table);
            break;
        }
        case MeasureFlag::InferredBuiltins: {
            MeasureTable table(fileBase.empty() ? "" : fileBase + "_bltFuns",
                               MeasureHeader("Caller", "Inferred", "Calls"));
            tables.emplace(flag, table);
            break;
        }
        case MeasureFlag::LazyArgs: {
            MeasureTable table(
                fileBase.empty() ? "" : fileBase + "_lazyArgs",
                MeasureHeader("Closure", "Optimized", "Initial"));
            tables.emplace(flag, table);
            break;
        }
        default:
            assert(false);
        }
    }
}

bool MeasureData::hasTable(MeasureFlag flag) const {
    return tables.count(flag);
}

MeasureTable& MeasureData::table(MeasureFlag flag) {
    assert(hasTable(flag));
    return tables.at(flag);
}

void MeasureData::reset() {
    for (auto it = tables.begin(); it != tables.end(); it++) {
        it->second.reset();
    }
}

void MeasureData::flush() const {
    for (auto it = tables.begin(); it != tables.end(); it++) {
        it->second.flush();
    }
}

void Measurer::recordClosureStart(Code* code, SEXP ast, bool isInline) {
    if (data.hasTable(MeasureFlag::Envs)) {
        MeasureTable& table = data.table(MeasureFlag::Envs);
        MeasureRow& row = table.row(code, ast, true);
        row.second++;
    }
}

void Measurer::recordInlineClosureStart(const char* name, void* entry) {
    if (data.hasTable(MeasureFlag::Envs)) {
        MeasureTable& table = data.table(MeasureFlag::Envs);
        MeasureRow& row = table.row(name, entry, true);
        row.second++;
    }
}

void Measurer::recordClosureMkEnv(Code* code, bool beforeStart, SEXP ast) {
    if (data.hasTable(MeasureFlag::Envs)) {
        MeasureTable& table = data.table(MeasureFlag::Envs);
        MeasureRow& row = table.row(code, ast, beforeStart);
        row.first++;
    }
}

static MeasureFlags INFER_FUN_FLAGS =
    MeasureFlags(MeasureFlag::InferredFuns) | MeasureFlag::InferredBuiltins;

void Measurer::recordCallInferReg(Code* code) {
    if (data.hasTable(MeasureFlag::InferredFuns)) {
        MeasureTable& table = data.table(MeasureFlag::InferredFuns);
        MeasureRow& row = table.row(code, true);
        row.first++;
        row.second++;
    }
}

void Measurer::recordCallInferBuiltin(Code* code) {
    for (MeasureFlag flag : INFER_FUN_FLAGS) {
        if (!data.hasTable(flag))
            continue;
        MeasureTable& table = data.table(flag);
        MeasureRow& row = table.row(code, true);
        row.first++;
        row.second++;
    }
}

void Measurer::recordCallInferFail(Code* code) {
    for (MeasureFlag flag : INFER_FUN_FLAGS) {
        if (!data.hasTable(flag))
            continue;
        MeasureTable& table = data.table(flag);
        MeasureRow& row = table.row(code, true);
        row.second++;
    }
}

void Measurer::recordCompiled(pir::ClosureVersion* code) {
    if (data.hasTable(MeasureFlag::LazyArgs)) {
        MeasureTable& table = data.table(MeasureFlag::LazyArgs);
        MeasureRow& row = table.row(code, true);
        row.second = pir::Query::lazyArgs(code);
        row.first = row.second;
    }
}

void Measurer::recordOptimized(pir::ClosureVersion* code) {
    if (data.hasTable(MeasureFlag::LazyArgs)) {
        MeasureTable& table = data.table(MeasureFlag::LazyArgs);
        MeasureRow& row = table.row(code, true);
        row.first = pir::Query::lazyArgs(code);
    }
}

}; // namespace rir
