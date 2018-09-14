#include "stream_logger.h"
#include "../pir/pir_impl.h"
#include "runtime/Function.h"
#include "utils/Pool.h"

#include <fstream>
#include <iomanip>
#include <sstream>

namespace rir {
namespace pir {

uint64_t StreamLogger::logId = 0;

StreamLogger::~StreamLogger() {
    while (!streams.empty()) {
        auto it = streams.begin();
        (*it->second).flush();
        streams.erase(it);
    }
}

void StreamLogger::flush() {
    for (auto& e : streams) {
        (*e.second).flush();
    }
}

FileLogStream::~FileLogStream() { fstream.close(); }

LogStream& StreamLogger::begin(Closure* cls, const std::string& name) {
    assert(!streams.count(cls) && "You already started this function");
    std::stringstream id;
    id << name;
    if (name.empty())
        id << "?";
    id << "_" << cls->rirVersion();

    if (options.includes(DebugFlag::PrintIntoFiles)) {
        std::stringstream filename;
        filename << "pir-function-" << std::setfill('0') << std::setw(5)
                 << logId++ << "-" << id.str() << ".log";
        streams.emplace(cls,
                        new FileLogStream(options, id.str(), filename.str()));
    } else {
        if (options.includes(DebugFlag::PrintIntoStdout))
            streams.emplace(cls, new LogStream(options, id.str()));
        else
            streams.emplace(cls, new BufferedLogStream(options, id.str()));
    }

    auto& logger = get(cls);

    if (options.includes(DebugFlag::PrintEarlyRir)) {
        logger.preparePrint();
        logger.section("Original version");
        cls->rirVersion()->disassemble(logger.out);
        logger.out << "\n";
    }

    return logger;
}

void LogStream::compilationEarlyPir(Closure* closure) {
    if (options.includes(DebugFlag::PrintEarlyPir)) {
        preparePrint();
        section("Compiled to PIR Version");
        closure->print(out);
    }
}

void LogStream::pirOptimizationsFinished(Closure* closure) {
    if (options.includes(DebugFlag::PrintPirAfterOpt)) {
        preparePrint();
        section("PIR Version After Optimizations");
        closure->print(out);
        out << "\n";
    }
}

void LogStream::pirOptimizations(Closure* closure, const std::string& pass,
                                 size_t passnr) {
    if (options.includes(DebugFlag::PrintOptimizationPasses)) {
        preparePrint();
        std::stringstream ss;
        ss << pass << ": == " << passnr;
        section(ss.str());
        closure->print(out);
    }
}

void LogStream::finalRIR(Function* fun) {
    if (options.includes(DebugFlag::PrintFinalRir)) {
        preparePrint();
        section("Final RIR");
        fun->disassemble(out);
        out << "\n";
    }
}

void LogStream::afterAllocator(Code* code,
                               std::function<void(std::ostream&)> allocDebug) {
    if (options.includes(DebugFlag::PrintAllocator)) {
        preparePrint();
        section("PIR SSA allocator");
        code->print(out);
        out << "\n";
        allocDebug(out);
    }
}

void LogStream::CSSA(Code* code) {
    if (options.includes(DebugFlag::PrintCSSA)) {
        preparePrint();
        section("CSSA Version");
        code->print(out);
        out << "\n";
    }
}

void LogStream::finalPIR(Closure* code) {
    if (options.includes(DebugFlag::PrintFinalPir)) {
        preparePrint();
        section("Final PIR Version");
        code->print(out);
        out << "\n";
    }
}

void LogStream::unsupportedBC(const std::string& warning, const rir::BC& bc) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        preparePrint();
        out << "Warning: " << warning << ": ";
        if (bc.bc == Opcode::guard_fun_) {
            out << CHAR(
                PRINTNAME(rir::Pool::get(bc.immediate.guard_fun_args.name)));
        } else {
            bc.print(out);
        }
        out << "\n";
    }
}

void StreamLogger::warn(const std::string& msg) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        std::cerr << "Warning: " << msg << "\n";
    }
}

void LogStream::warn(const std::string& msg) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        preparePrint();
        out << "Warning: " << msg << " in " << id << "\n";
    }
}

void LogStream::failed(const std::string& msg) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        preparePrint();
        out << "Failed: " << msg << " in " << id << "\n";
    }
}

static const std::string RED = "\033[1;31m";
static const std::string CLEAR = "\033[1;31m";

void LogStream::highlightOn() { out << RED; }

void LogStream::highlightOff() { out << CLEAR; }

void LogStream::header() {
    highlightOn();
    out << "┌";
    for (size_t i = 0; i < 78; ++i)
        out << "─";
    out << "┐\n";
    out << "│ " << std::left << std::setw(77) << id << "│\n";
    highlightOff();
}

void LogStream::footer() {
    highlightOn();
    out << "│ " << std::left << std::setw(77) << id << "│\n";
    out << "└";
    for (size_t i = 0; i < 78; ++i)
        out << "─";
    out << "┘\n";
    highlightOff();
}

void LogStream::section(const std::string& title) {
    highlightOn();
    preparePrint();
    out << "├────── " << title;
    if (options.includes(DebugFlag::PrintIntoStdout))
        out << "(" << id << ")";
    out << "\n";
    highlightOff();
}

} // namespace pir
} // namespace rir
