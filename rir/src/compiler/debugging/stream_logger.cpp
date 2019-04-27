#include "stream_logger.h"
#include "../analysis/query.h"
#include "../pir/pir_impl.h"
#include "../translations/pir_translator.h"
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

LogStream& StreamLogger::begin(ClosureVersion* cls) {
    assert(!streams.count(cls) && "You already started this function");

    if (options.includes(DebugFlag::PrintIntoFiles)) {
        std::stringstream filename;
        filename << cls->owner()->name() << "-pir-function-"
                 << std::setfill('0') << std::setw(5) << logId++;
        if (options.style == DebugStyle::Standard)
            filename << ".log";
        else
            filename << ".dot";
        streams.emplace(cls, new FileLogStream(options, cls, filename.str()));
    } else {
        if (options.includes(DebugFlag::PrintIntoStdout))
            streams.emplace(cls, new LogStream(options, cls));
        else
            streams.emplace(cls, new BufferedLogStream(options, cls));
    }

    auto& logger = get(cls);

    if (options.includes(DebugFlag::PrintEarlyRir)) {
        logger.preparePrint();
        logger.section("Original version");
        cls->owner()->rirFunction()->disassemble(logger.out);
        logger.out << "\n";
    }

    return logger;
}

void LogStream::compilationEarlyPir(ClosureVersion* closure) {
    if (options.includes(DebugFlag::PrintEarlyPir)) {
        preparePrint();
        section("Compiled to PIR Version");
        closure->print(options.style, out, tty(),
                       options.includes(DebugFlag::OmitDeoptBranches));
    }
}

void LogStream::pirOptimizationsFinished(ClosureVersion* closure) {
    auto name = version->name();
    if (options.includes(DebugFlag::PrintPirAfterOpt) &&
        std::regex_match(name.begin(), name.end(), options.functionFilter)) {
        preparePrint();
        section("PIR Version After Optimizations");
        closure->print(options.style, out, tty(),
                       options.includes(DebugFlag::OmitDeoptBranches));
        out << "\n";
    }
}

static bool shouldLog(ClosureVersion* version, const PirTranslator* pass,
                      const DebugOptions& options) {
    auto name = version->name();
    if (options.includes(DebugFlag::PrintOptimizationPasses) &&
        !pass->isPhaseMarker()) {
        auto passName = pass->getName();
        return std::regex_match(name.begin(), name.end(),
                                options.functionFilter) &&
               std::regex_match(passName.begin(), passName.end(),
                                options.passFilter);
    }
    if (options.includes(DebugFlag::PrintOptimizationPhases) &&
        pass->isPhaseMarker()) {
        auto name = version->name();
        return std::regex_match(name.begin(), name.end(),
                                options.functionFilter);
    }
    return false;
}

void LogStream::pirOptimizationsHeader(ClosureVersion* closure,
                                       const PirTranslator* pass,
                                       size_t passnr) {
    if (shouldLog(closure, pass, options)) {
        preparePrint();
        std::stringstream ss;
        ss << pass->getName() << ": == " << passnr;
        section(ss.str());
    }
}

void LogStream::pirOptimizations(ClosureVersion* closure,
                                 const PirTranslator* pass) {
    if (shouldLog(closure, pass, options)) {
        closure->print(options.style, out, tty(),
                       options.includes(DebugFlag::OmitDeoptBranches));
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
        code->printCode(out, tty(),
                        options.includes(DebugFlag::OmitDeoptBranches));
        out << "\n";
        allocDebug(out);
    }
}

void LogStream::CSSA(Code* code) {
    if (options.includes(DebugFlag::PrintCSSA)) {
        preparePrint();
        section("CSSA Version");
        code->printCode(out, tty(),
                        options.includes(DebugFlag::OmitDeoptBranches));
        out << "\n";
    }
}

void LogStream::finalPIR(ClosureVersion* code) {
    if (options.includes(DebugFlag::PrintFinalPir)) {
        preparePrint();
        section("Final PIR Version");
        code->print(options.style, out, tty(),
                    options.includes(DebugFlag::OmitDeoptBranches));
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
        out << "Warning: " << msg << " in " << version->name() << "\n";
    }
}

void LogStream::failed(const std::string& msg) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        preparePrint();
        out << "Failed: " << msg << " in " << version->name() << "\n";
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
    std::string c = options.includes(DebugFlag::PrintIntoFiles) &&
                            options.style != DebugStyle::Standard
                        ? "// "
                        : "";

    highlightOn();
    out << "\n" << c << "┌";
    for (size_t i = 0; i < 78; ++i)
        out << "─";
    std::stringstream assumptions;
    assumptions << "Assumptions: " << version->assumptions();
    std::stringstream properties;
    properties << "Properties:  " << version->properties;
    out << "┐\n";
    out << c << "│ " << std::left << std::setw(77) << version->name() << "│\n";
    out << c << "│ " << std::left << std::setw(77) << assumptions.str()
        << "│\n";
    if (properties.str() != "")
        out << c << "│ " << std::left << std::setw(77) << properties.str()
            << "│\n";
    highlightOff();
}

void LogStream::footer() {
    std::string c = options.includes(DebugFlag::PrintIntoFiles) &&
                            options.style != DebugStyle::Standard
                        ? "// "
                        : "";
    highlightOn();
    out << c << "│ " << std::left << std::setw(77) << version->name() << "│\n";
    out << c << "└";
    for (size_t i = 0; i < 78; ++i)
        out << "─";
    out << "┘\n";
    highlightOff();
}

void LogStream::section(const std::string& title) {
    std::string c = options.includes(DebugFlag::PrintIntoFiles) &&
                            options.style != DebugStyle::Standard
                        ? "// "
                        : "";
    highlightOn();
    preparePrint();
    out << c << "├────── " << title;
    if (options.includes(DebugFlag::PrintIntoStdout))
        out << "(" << version->name() << ")";
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
