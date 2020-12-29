#include "stream_logger.h"
#include "compiler/opt/pass.h"
#include "compiler/pir/pir_impl.h"
#include "runtime/Function.h"
#include "utils/Pool.h"
#include "utils/Terminal.h"
#include "utils/filesystem.h"

#include <fstream>
#include <iomanip>
#include <sstream>
#include <sys/stat.h>
namespace rir {
namespace pir {

static const std::string LOG_FOLDER = "log/";

size_t StreamLogger::logId = 0;
bool clearedLogFolderThisSession = false;

static bool usesLogFolder(const DebugOptions& options) {
    return options.includes(DebugFlag::PrintPassesIntoFolders) ||
           options.includes(DebugFlag::PrintIntoFiles);
}

StreamLogger::StreamLogger(const DebugOptions& options) : options(options) {
    if (!clearedLogFolderThisSession && usesLogFolder(options)) {
        clearOrCreateDirectory(LOG_FOLDER.c_str());
        clearedLogFolderThisSession = true;
    }
}

StreamLogger::~StreamLogger() {
    while (!streams.empty()) {
        auto it = streams.begin();
        it->second.flush();
        streams.erase(it);
    }
}

FileLogStream::~FileLogStream() { fstream.close(); }
bool BufferedLogStream::tty() { return ConsoleColor::isTTY(actualOut); }
bool SimpleLogStream::tty() { return ConsoleColor::isTTY(out); }

static std::string closureLogPath(size_t logId, const ClosureVersion* cls) {
    std::stringstream filePath;
    filePath << LOG_FOLDER << cls->owner()->name() << "-pir-function-"
             << std::setfill('0') << std::setw(5) << logId;
    return filePath.str();
}

static std::string logFileExtension(DebugOptions options) {
    if (options.style == DebugStyle::Standard)
        return ".log";
    else
        return ".dot";
}

ClosureStreamLogger& StreamLogger::begin(ClosureVersion* cls) {
    assert(!streams.count(cls) && "You already started this function");
    std::string basePath = closureLogPath(logId, cls);

    // Create inner stream
    LogStream* logStream;
    if (options.includes(DebugFlag::PrintIntoFiles)) {
        std::stringstream filePath;
        filePath << basePath << logFileExtension(options);
        logStream = new FileLogStream(filePath.str());
    } else {
        if (options.includes(DebugFlag::PrintIntoStdout))
            logStream = new SimpleLogStream;
        else
            logStream = new BufferedLogStream;
    }

    // Clear and create folder for individual pass streams
    if (options.includes(DebugFlag::PrintPassesIntoFolders)) {
        clearOrCreateDirectory(basePath.c_str());
    }

    // Add stream logger wrapper
    streams.emplace(cls,
                    ClosureStreamLogger(options, logId, cls,
                                        std::shared_ptr<LogStream>(logStream)));
    ClosureStreamLogger& logger = streams.at(cls);
    // Increment log id
    logId++;

    // Print early rir
    if (options.includes(DebugFlag::PrintEarlyRir)) {
        logger.preparePrint();
        logger.section("Original version");
        cls->owner()->rirFunction()->disassemble(logger.out().out);
        logger.out() << "\n";
    }

    return logger;
}

static std::string passLogFileName(size_t number) {
    std::stringstream name;
    name << std::setfill('0') << std::setw(5) << number;
    return name.str();
}

PassStreamLogger ClosureStreamLogger::forPass(size_t number) {
    if (options.includes(DebugFlag::PrintPassesIntoFolders)) {
        std::stringstream path;
        path << closureLogPath(logId, version) << "/" << passLogFileName(number)
             << logFileExtension(options);
        FileLogStream* stream = new FileLogStream(path.str());
        return PassStreamLogger(number, *this,
                                std::shared_ptr<LogStream>(stream));
    } else {
        return PassStreamLogger(number, *this, _out);
    }
}

PassStreamLogger::PassStreamLogger(size_t number, ClosureStreamLogger& parent,
                                   const std::shared_ptr<LogStream>& out)
    : GenericStreamLogger(parent.options, parent.version, out), number(number),
      parent(parent) {}

void PassStreamLogger::preparePrint() { parent.preparePrint(); }

void ClosureStreamLogger::compilationEarlyPir(ClosureVersion* closure) {
    if (options.includes(DebugFlag::PrintEarlyPir)) {
        preparePrint();
        section("Compiled to PIR Version");
        closure->print(options.style, out().out, out().tty(),
                       options.includes(DebugFlag::OmitDeoptBranches));
    }
}

void ClosureStreamLogger::pirOptimizationsFinished(ClosureVersion* closure) {
    auto name = version->name();
    if (options.includes(DebugFlag::PrintPirAfterOpt) &&
        std::regex_match(name.begin(), name.end(), options.functionFilter)) {
        preparePrint();
        section("PIR Version After Optimizations");
        closure->print(options.style, out().out, out().tty(),
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

void GenericStreamLogger::header() {
    std::string c = options.includes(DebugFlag::PrintIntoFiles) &&
                            options.style != DebugStyle::Standard
                        ? "// "
                        : "";

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

void GenericStreamLogger::footer() {
    std::string c = options.includes(DebugFlag::PrintIntoFiles) &&
                            options.style != DebugStyle::Standard
                        ? "// "
                        : "";
    highlightOn();
    out() << c << "│ " << std::left << std::setw(77) << version->name()
          << "│\n";
    out() << c << "└";
    for (size_t i = 0; i < 78; ++i)
        out() << "─";
    out() << "┘\n";
    highlightOff();
}

void GenericStreamLogger::section(const std::string& title) {
    std::string c = options.includes(DebugFlag::PrintIntoFiles) &&
                            options.style != DebugStyle::Standard
                        ? "// "
                        : "";
    highlightOn();
    preparePrint();
    out() << c << "├────── " << title;
    if (options.includes(DebugFlag::PrintIntoStdout))
        out() << "(" << version->name() << ")";
    out() << "\n";
    highlightOff();
}

void GenericStreamLogger::warn(const std::string& msg) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        preparePrint();
        out() << "Warning: " << msg << " in " << version->name() << "\n";
    }
}

void GenericStreamLogger::failed(const std::string& msg) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        preparePrint();
        out() << "Failed: " << msg << " in " << version->name() << "\n";
    }
}

void GenericStreamLogger::highlightOn() {
    if (out().tty())
        ConsoleColor::red(out().out);
}

void GenericStreamLogger::highlightOff() {
    if (out().tty())
        ConsoleColor::clear(out().out);
}

void PassStreamLogger::pirOptimizationsHeader(const Pass* pass) {
    if (shouldLog(version, pass, options)) {
        preparePrint();
        std::stringstream ss;
        ss << pass->getName() << ": == " << number;
        section(ss.str());
    }
}

void PassStreamLogger::pirOptimizations(const Pass* pass) {
    if (shouldLog(version, pass, options)) {
        if (!options.includes(DebugFlag::OnlyChanges) ||
            pass->changedAnything())
            version->print(options.style, out().out, out().tty(),
                           options.includes(DebugFlag::OmitDeoptBranches));
    }
}

void ClosureStreamLogger::CSSA(Code* code) {
    if (options.includes(DebugFlag::PrintCSSA)) {
        preparePrint();
        section("CSSA Version");
        code->printCode(out().out, out().tty(),
                        options.includes(DebugFlag::OmitDeoptBranches));
        out() << "\n";
    }
}

void ClosureStreamLogger::finalPIR(ClosureVersion* code) {
    if (options.includes(DebugFlag::PrintFinalPir)) {
        preparePrint();
        section("Final PIR Version");
        code->print(options.style, out().out, out().tty(),
                    options.includes(DebugFlag::OmitDeoptBranches));
        out() << "\n";
    }
}

void ClosureStreamLogger::unsupportedBC(const std::string& warning,
                                        const rir::BC& bc) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        preparePrint();
        out() << "Warning: " << warning << ": ";
        if (bc.bc == Opcode::guard_fun_) {
            out() << CHAR(
                PRINTNAME(rir::Pool::get(bc.immediate.guard_fun_args.name)));
        } else {
            bc.print(out().out);
        }
        out() << "\n";
    }
}

void StreamLogger::warn(const std::string& msg) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        std::cerr << "Warning: " << msg << "\n";
    }
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
        e.second.flush();
    }
}

} // namespace pir
} // namespace rir
