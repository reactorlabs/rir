#include "loggers.h"

#include "compiler/log/loggers.h"
#include "compiler/opt/pass.h"
#include "compiler/pir/closure.h"
#include "compiler/pir/closure_version.h"
#include "runtime/Function.h"
#include "sinks.h"
#include "utils/Pool.h"
#include "utils/Terminal.h"
#include "utils/filesystem.h"

#include <fstream>
#include <iomanip>
#include <memory>
#include <sstream>
#include <sys/stat.h>

#include <stdlib.h>

namespace rir {
namespace pir {

static std::string passLogFileName(size_t number) {
    std::stringstream name;
    name << std::setfill('0') << std::setw(5) << number;
    return name.str();
}

PassLog ClosureLog::forPass(size_t number, const std::string& kind) {
    bool dot = options.style != DebugStyle::Standard;
    if (options.multipleFiles()) {
        std::stringstream path;
        path << basePath << "/" << passLogFileName(number) << "-" << kind;
        FileLogStream* pirStream;
        if (!dot)
            pirStream = new FileLogStream(path.str() + ".log");
        else
            pirStream = new DotFileLogStream(path.str() + ".dot");
        return PassLog(number, *this, std::shared_ptr<LogStream>(pirStream));
    } else {
        return PassLog(number, *this, _out);
    }
}

PassLog::PassLog(size_t number, ClosureLog& parent,
                 std::shared_ptr<LogStream> out)
    : AbstractLog(parent.options, parent.version, out), number(number),
      parent(parent) {}

void PassLog::preparePrint() { parent.preparePrint(); }

void PassLog::compilationEarlyPir() {
    if (options.includes(DebugFlag::PrintEarlyPir)) {
        preparePrint();
        section("Compiled to PIR Version");
        version->print(options.style, out().out(), out().tty(),
                       options.includes(DebugFlag::OmitDeoptBranches));
        flush();
    }
}

void PassLog::pirOptimizationsFinished() {
    auto name = version->name();
    if (options.includes(DebugFlag::PrintPirAfterOpt) &&
        std::regex_match(name.begin(), name.end(), options.functionFilter)) {
        preparePrint();
        section("PIR Version After Optimizations");
        version->print(options.style, out().out(), out().tty(),
                       options.includes(DebugFlag::OmitDeoptBranches));
        out() << "\n";
    }
}

static bool shouldLog(const ClosureVersion* version, const Pass* pass,
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
        return std::regex_match(name.begin(), name.end(),
                                options.functionFilter);
    }
    return false;
}

void AbstractLog::flush() {
    if (_out)
        _out->flush();
}

void AbstractLog::header() {
    std::string c = out().comment();
    highlightOn();
    out() << "\n" << c << "┌";
    for (size_t i = 0; i < 78; ++i)
        out() << "─";
    std::stringstream assumptions;
    assumptions << "Context: " << version->context();
    std::stringstream properties;
    properties << "Properties:  " << version->properties;
    out() << "┐\n";
    out() << c << "│ " << std::left << std::setw(77) << version->name()
          << "│\n";
    out() << c << "│ " << std::left << std::setw(77) << assumptions.str()
          << "│\n";
    if (properties.str() != "")
        out() << c << "│ " << std::left << std::setw(77) << properties.str()
              << "│\n";
    highlightOff();
}

void AbstractLog::footer() {
    std::string c = out().comment();
    highlightOn();
    out() << c << "│ " << std::left << std::setw(77) << version->name()
          << "│\n";
    out() << c << "└";
    for (size_t i = 0; i < 78; ++i)
        out() << "─";
    out() << "┘\n";
    highlightOff();
}

void AbstractLog::section(const std::string& title) {
    std::string c = out().comment();
    highlightOn();
    preparePrint();
    out() << c << "├────── " << title;
    if (options.includes(DebugFlag::PrintToStdout) &&
        options.includes(DebugFlag::PrintUnbuffered))
        out() << "(" << version->name() << ")";
    out() << "\n";
    highlightOff();
}

void AbstractLog::warn(const std::string& msg) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        preparePrint();
        out() << "Warning: " << msg << " in " << version->name() << "\n";
    }
}

void AbstractLog::failed(const std::string& msg) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        preparePrint();
        out() << "Failed: " << msg << " in " << version->name() << "\n";
    }
}

void AbstractLog::highlightOn() {
    if (out().tty())
        ConsoleColor::red(out().out());
}

void AbstractLog::highlightOff() {
    if (out().tty())
        ConsoleColor::clear(out().out());
}

void PassLog::pirOptimizationsHeader(const Pass* pass) {
    if (shouldLog(version, pass, options)) {
        preparePrint();
        std::stringstream ss;
        ss << pass->getName() << ": == " << number;
        parent.section(ss.str());
    }
}

void PassLog::pirOptimizations(const Pass* pass) {
    if (shouldLog(version, pass, options)) {
        if (!options.includes(DebugFlag::OnlyChanges) ||
            pass->changedAnything())
            version->print(options.style, out().out(), out().tty(),
                           options.includes(DebugFlag::OmitDeoptBranches));
    }
}

void ClosureLog::CSSA(Code* code) {
    if (options.includes(DebugFlag::PrintCSSA)) {
        preparePrint();
        section("CSSA Version");
        code->printCode(out().out(), out().tty(),
                        options.includes(DebugFlag::OmitDeoptBranches));
        out() << "\n";
    }
}

void PassLog::finalPIR() {
    if (options.includes(DebugFlag::PrintFinalPir)) {
        preparePrint();
        section("Final PIR Version");
        version->print(options.style, out().out(), out().tty(),
                       options.includes(DebugFlag::OmitDeoptBranches));
        out() << "\n";
    }
}

void ClosureLog::LLVMBitcode(const LLVMBitcodePrint& print) {
    if (options.includes(DebugFlag::PrintLLVM)) {
        preparePrint();
        section("LLVM Bitcode");
        print(out().out(), out().tty());
        out() << "\n";
    }
}

void ClosureLog::unsupportedBC(const std::string& warning, const rir::BC& bc) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        preparePrint();
        out() << "Warning: " << warning << ": ";
        if (bc.bc == Opcode::guard_fun_) {
            out() << CHAR(
                PRINTNAME(rir::Pool::get(bc.immediate.guard_fun_args.name)));
        } else {
            bc.print(out().out());
        }
        out() << "\n";
    }
}

} // namespace pir
} // namespace rir
