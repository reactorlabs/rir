#include "stream_logger.h"
#include "../pir/pir_impl.h"
#include "runtime/Function.h"
#include "utils/Pool.h"
#include "utils/Terminal.h"

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

FileLogStream::~FileLogStream() { fstream.close(); }

bool LogStream::tty() { return ConsoleColor::isTTY(out); }
bool BufferedLogStream::tty() { return ConsoleColor::isTTY(actualOut); }

LogStream& StreamLogger::begin(Closure* cls) {
    assert(!streams.count(cls) && "You already started this function");

    if (options.includes(DebugFlag::PrintIntoFiles)) {
        std::stringstream filename;
        filename << "pir-function-" << std::setfill('0') << std::setw(5)
                 << logId++ << "-" << cls->name << ".log";
        streams.emplace(cls,
                        new FileLogStream(options, cls->name, filename.str()));
    } else {
        if (options.includes(DebugFlag::PrintIntoStdout))
            streams.emplace(cls, new LogStream(options, cls->name));
        else
            streams.emplace(cls, new BufferedLogStream(options, cls->name));
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
        closure->print(out, tty());
    }
}

void LogStream::pirOptimizationsFinished(Closure* closure) {
    if (options.includes(DebugFlag::PrintPirAfterOpt)) {
        preparePrint();
        section("PIR Version After Optimizations");
        closure->print(out, tty());
        out << "\n";
    }
}

void LogStream::pirOptimizationsHeader(Closure* closure,
                                       const std::string& pass, size_t passnr) {
    if (options.includes(DebugFlag::PrintOptimizationPasses)) {
        preparePrint();
        std::stringstream ss;
        ss << pass << ": == " << passnr;
        section(ss.str());
    }
}

void LogStream::pirOptimizations(Closure* closure) {
    if (options.includes(DebugFlag::PrintOptimizationPasses)) {
        closure->print(out, tty());
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
        code->printCode(out, tty());
        out << "\n";
        allocDebug(out);
    }
}

void LogStream::CSSA(Code* code) {
    if (options.includes(DebugFlag::PrintCSSA)) {
        preparePrint();
        section("CSSA Version");
        code->printCode(out, tty());
        out << "\n";
    }
}

void LogStream::finalPIR(Closure* code) {
    if (options.includes(DebugFlag::PrintFinalPir)) {
        preparePrint();
        section("Final PIR Version");
        code->print(out, tty());
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

void LogStream::highlightOn() {
    if (tty())
        ConsoleColor::red(out);
}
void LogStream::highlightOff() {
    if (tty())
        ConsoleColor::clear(out);
}

void LogStream::header() {
    highlightOn();
    out << "\n┌";
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

void StreamLogger::title(const std::string& msg) {
    if (!options.includes(DebugFlag::PrintIntoFiles) &&
        (options.intersects(PrintDebugPasses) ||
         options.includes(DebugFlag::ShowWarnings))) {
        if (ConsoleColor::isTTY(std::cout))
            ConsoleColor::blue(std::cout);
        int l = 36 - (int)msg.length() / 2;
        int r = l - msg.length() % 2;
        std::cout << "\n╞";
        for (int i = 0; i < l; ++i)
            std::cout << "═";
        std::cout << "╡  ";
        if (ConsoleColor::isTTY(std::cout))
            ConsoleColor::clear(std::cout);
        std::cout << msg;
        if (ConsoleColor::isTTY(std::cout))
            ConsoleColor::blue(std::cout);
        std::cout << "  ╞";
        for (int i = 0; i < r; ++i)
            std::cout << "═";
        std::cout << "╡\n";
        if (ConsoleColor::isTTY(std::cout))
            ConsoleColor::clear(std::cout);
    }
}

void StreamLogger::flush() {
    for (auto& e : streams) {
        (*e.second).flush();
    }
}

} // namespace pir
} // namespace rir
