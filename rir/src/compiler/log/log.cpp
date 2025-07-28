#include "log.h"

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

static std::string debugLocation = "";
size_t Log::logId = 0;
bool clearedLogFolderThisSession = false;

static bool anyLoggingEnabled(const DebugOptions& options) {
    return !options.flags.empty();
}

static bool usesLogFolder(const DebugOptions& options) {
    return !options.includes(DebugFlag::PrintToStdout);
}

Log::Log(const DebugOptions& options) : options(options) {
    if (anyLoggingEnabled(options) && usesLogFolder(options) &&
        !debugLocation.size()) {
        auto t = std::time(nullptr);
        auto tm = *std::localtime(&t);
        std::stringstream ss;
        auto time = std::put_time(&tm, "%y%m%d-%H%M%S");

        auto env_location = std::getenv("PIR_DEBUG_FOLDER");
        if (env_location != nullptr) {
            ss << env_location << "-" << time << "/";
            debugLocation = ss.str();
            auto ret = mkdir(debugLocation.c_str(), 0700);
            if (ret != 0) {
                perror("PIR debug folder:");
            }
        } else {
            ss << "compiler-log-" << time << "-XXXXXX";
            debugLocation = createTmpDirectory(ss.str()) + "/";
        }
    }
}

Log::~Log() {
    while (!streams.empty()) {
        auto it = streams.begin();
        it->second.flush();
        streams.erase(it);
    }
}

static std::string closureLogPath(const std::string& debugLocation,
                                  size_t logId, const ClosureVersion* cls) {
    std::stringstream filePath;
    filePath << debugLocation << std::setfill('0') << std::setw(3)
             << report::currentSessionId() << "-" << std::setfill('0')
             << std::setw(5) << logId << "-" << cls->owner()->name()
             << "-pir-function";

    return filePath.str();
}

ClosureLog& Log::open(ClosureVersion* cls) {
    assert(!streams.count(cls) && "You already started this function");
    std::string basePath = closureLogPath(debugLocation, logId, cls);

    // Create inner stream
    LogStream* logStream = nullptr;

    if (anyLoggingEnabled(options)) {
        if (usesLogFolder(options)) {
            logStream = new FileLogStream(basePath + ".log");
        } else {
            if (options.includes(DebugFlag::PrintUnbuffered))
                logStream = new UnbufferedLogStream;
            else
                logStream = new BufferedLogStream;
        }
    }

    // Clear and create folder for individual pass streams
    if (options.multipleFiles())
        clearOrCreateDirectory(basePath.c_str());

    // Add stream logger wrapper
    streams.emplace(cls, ClosureLog(options, cls,
                                    std::shared_ptr<LogStream>(logStream),
                                    basePath));
    ClosureLog& logger = streams.at(cls);
    // Increment log id
    logId++;

    // Print early rir
    if (options.includes(DebugFlag::PrintEarlyRir)) {
        logger.preparePrint();
        logger.section("Original version");
        cls->owner()->rirFunction()->disassemble(logger.out().out());
        logger.out() << "\n";
    }

    return logger;
}

void Log::warn(const std::string& msg) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        std::cerr << "Warning: " << msg << "\n";
    }
}

void Log::title(const std::string& msg) {
    if (options.includes(DebugFlag::PrintToStdout) &&
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

void Log::flushAll() {
    for (auto& e : streams) {
        e.second.flush();
    }
}

} // namespace pir
} // namespace rir
