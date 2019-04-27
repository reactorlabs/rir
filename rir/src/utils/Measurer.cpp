#include "Measurer.h"
#include "compiler/analysis/query.h"
#include "compiler/pir/pir_impl.h"
#include <fstream>
#include <sstream>
#include <stdio>

namespace rir {

using namespace pir;

MeasureTable::MeasureTable(std::function<unsigned(ClosureVersion*)> measureFunc,
                           std::string fileName)
    : measureFunc(measureFunc), fileName(fileName) {
    if (!fileName.empty()) {
        std::ifstream inp(fileName.c_str());
        if (inp.good()) // File exists
            readCsv(inp);
    }
}

void MeasureTable::readCsv(std::istream& in) {
    // Remove header
    std::string head;
    std::getline(in, head, '\n');
    if (head.empty()) {
        std::cerr << "invalid csv file: " << fileName << "\n";
        assert(false);
    }

    while (in.peek() != EOF) {
        std::string key;
        unsigned initial;
        unsigned optimized;
        std::getline(in, key, ',');
        // From
        // https://stackoverflow.com/questions/1798112/removing-leading-and-trailing-spaces-from-a-string
        key.erase(std::find_if(key.rbegin(), key.rend(),
                               std::bind1st(std::not_equal_to<char>(), ' '))
                      .base(),
                  key.end());
        in >> std::ws >> initial;
        char delim = in.get();
        assert(delim == ',');
        in >> std::ws >> optimized;
        delim = in.get();
        assert(delim == '\n');

        MeasureRow row(initial, optimized);
        rows.emplace(key, row);
    }
}

void MeasureTable::writeCsv(std::ostream& out) const {
    unsigned pad1 = std::string("Closure").size();
    unsigned pad2 = std::string("Initial").size();
    unsigned pad3 = std::string("Optimized").size();
    for (auto row : rows) {
        pad1 = std::max(pad1, (unsigned)row.first.size());
        pad2 = std::max(pad2, (unsigned)log10((double)row.second.initial));
        pad3 = std::max(pad3, (unsigned)log10((double)row.second.optimized));
    }
    out << std::left << std::setw(pad1) << "Closure"
        << ", " << std::left << std::setw(pad2) << "Initial"
        << ", " << std::left << std::setw(pad3) << "Optimized"
        << "\n";
    for (auto row : rows) {
        out << std::left << std::setw(pad1) << row.first << ", " << std::right
            << std::setw(pad2) << row.second.initial << ", " << std::right
            << std::setw(pad3) << row.second.optimized << "\n";
    }
    out.flush();
}

void MeasureTable::recordInitial(ClosureVersion* code) {
    // Dirty hack to not fail when the closure is deallocated. Would reuse a
    // previous deallocated closure if it has the exact same name and hash
    std::string key = code->owner()->name();
    if (rows.count(key)) {
        // Recompiled
        rows.erase(key);
    }
    MeasureRow row(measureFunc(code));
    rows.emplace(key, row);
    update();
}

void MeasureTable::recordNew(ClosureVersion* code) {
    std::string key = code->owner()->name();
    if (!rows.count(key)) {
        std::cerr << "optimized closure before compiling:" << code << "\n";
        assert(false);
    }
    rows.at(key).recordNew(measureFunc(code));
    update();
}

void MeasureTable::reset() {
    rows.clear();
    update();
}

void MeasureTable::update() {
    if (!fileName.empty()) {
        FILE* out = fopen(fileName.c_str(), "w");
        std::ofstream out(fileName.c_str(), std::ios_base::trunc);
        writeCsv(out);
    }
}

Measurer::Measurer(MeasureFlags flags, std::string fileBase) {
    for (MeasureFlag flag : flags) {
        switch (flag) {
        case MeasureFlag::Envs: {
            MeasureTable table(Query::mkEnvs,
                               fileBase.empty() ? "" : fileBase + "_envs.csv");
            tables.emplace(flag, table);
            break;
        }
        case MeasureFlag::Vars: {
            MeasureTable table(Query::envVars,
                               fileBase.empty() ? "" : fileBase + "_vars.csv");
            tables.emplace(flag, table);
            break;
        }
        case MeasureFlag::LazyArgs: {
            MeasureTable table(Query::lazyArgs,
                               fileBase.empty() ? ""
                                                : fileBase + "_lazyArgs.csv");
            tables.emplace(flag, table);
            break;
        }
        default:
            assert(false);
        }
    }
}

MeasureTable& Measurer::table(MeasureFlag flag) {
    if (!tables.count(flag))
        Rf_error("not measuring this flag");
    return tables.at(flag);
}

void Measurer::recordInitial(ClosureVersion* code) {
    for (auto it = tables.begin(); it != tables.end(); it++) {
        it->second.recordInitial(code);
    }
}

void Measurer::recordNew(ClosureVersion* code) {
    for (auto it = tables.begin(); it != tables.end(); it++) {
        it->second.recordNew(code);
    }
}

void Measurer::reset() {
    for (auto it = tables.begin(); it != tables.end(); it++) {
        it->second.reset();
    }
}

}; // namespace rir
