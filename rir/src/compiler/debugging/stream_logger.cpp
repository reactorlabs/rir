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
    for (auto const& fun2stream : streams) {
        finish(fun2stream.first, *fun2stream.second);
        if (options.includes(DebugFlag::PrintIntoFiles)) {
            std::stringstream filename;
            filename << ".pirlog"
                     << "-" << std::setfill('0') << std::setw(5) << logId++
                     << "-" << fun2stream.first;
            std::ofstream file(filename.str());
            file << fun2stream.second->rdbuf();
        }
        if (options.includes(DebugFlag::PrintIntoStdout)) {
            std::cout << fun2stream.second->rdbuf();
        }
    }
}

void StreamLogger::startLogging(rir::Function* function) {
    if (options.intersects(PrintDebugPasses)) {
        streams.emplace(function, std::make_unique<std::stringstream>());
        compilationInit(function);
    }
}

void StreamLogger::compilationInit(rir::Function* function) {
    header("Start compiling:", function, getLog(function), true);
    if (options.includes(DebugFlag::PrintEarlyRir)) {
        innerHeader(function, " Original version ");
        for (auto code : *function) {
            code->print(getLog(function));
        }
        getLog(function) << "\n";
    }
}

void StreamLogger::compilationEarlyPir(Closure& closure) {
    if (options.includes(DebugFlag::PrintEarlyPir)) {
        innerHeader(closure.rirVersion(), " Compiled to PIR Version ");
        closure.print(getLog(closure.rirVersion()));
    }
}

void StreamLogger::pirOptimizations(Closure& closure, const std::string& pass,
                                    size_t passnr) {
    if (options.includes(DebugFlag::PrintOptimizationPasses)) {
        std::stringstream ss;
        ss << pass << ": == " << passnr;
        innerHeader(closure.rirVersion(), ss.str());
        closure.print(getLog(closure.rirVersion()));
    }
}

void StreamLogger::rirFromPir(rir::Function* function) {
    if (options.includes(DebugFlag::PrintFinalRir)) {
        innerHeader(function, " Final RIR Version ");
        for (const auto code : *function) {
            code->print(getLog(function));
        }
        getLog(function) << "\n";
    }
}

void StreamLogger::afterCSSA(Closure& closure, const Code* code) {
    if (options.includes(DebugFlag::PrintCSSA)) {
        innerHeader(closure.rirVersion(), " PIR After Converting to CSSA ");
        code->print(getLog(closure.rirVersion()));
    }
}

void StreamLogger::afterAllocator(
    Closure& closure, std::function<void(std::ostream&)> maybePrint) {
    if (options.includes(DebugFlag::PrintAllocator)) {
        innerHeader(closure.rirVersion(), " PIR SSA allocator ");
        maybePrint(getLog(closure.rirVersion()));
    }
}

void StreamLogger::finalPIR(Closure& closure) {
    if (options.includes(DebugFlag::PrintFinalPir)) {
        innerHeader(closure.rirVersion(), " Final PIR Version ");
        closure.print(getLog(closure.rirVersion()));
    }
}

void StreamLogger::finish(const rir::Function* function, std::ostream& stream) {
    if (options.intersects(PrintDebugPasses)) {
        header("Finished compiling:", function, stream, false);
    }
}

void StreamLogger::warningBC(rir::Function* function, std::string warning,
                             rir::BC bc) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        getLog(function) << "Warning: " << warning << ": ";
        if (warning.compare(WARNING_GUARD_STRING) == 0) {
            getLog(function) << CHAR(
                PRINTNAME(rir::Pool::get(bc.immediate.guard_fun_args.name)));
        } else {
            bc.print(getLog(function));
        }
        getLog(function) << "\n";
    }
}

void StreamLogger::failCompilingPir(rir::Function* function) {
    if (options.includes(DebugFlag::ShowWarnings))
        getLog(function) << " Failed verification after p2r compile "
                         << function << "\n";
}

void StreamLogger::header(std::string header, const rir::Function* function,
                          std::ostream& stream, bool opening) {
    std::stringstream ss;
    ss << " " << header << " " << function << " ";
    stream << std::setfill('*') << std::left;
    if (opening)
        stream << "\n"
               << std::setw(70) << ""
               << "\n";
    else
        stream << "\n";
    stream << std::left << std::setw(21) << "" << std::setw(49) << ss.str()
           << "\n";
    if (!opening)
        stream << std::setw(70) << ""
               << "\n";
    stream << std::setfill(' ') << "\n";
}

void StreamLogger::innerHeader(rir::Function* function, std::string header) {
    getLog(function) << "\n"
                     << std::setfill('=') << std::setw(15) << "" << std::left
                     << std::setw(39) << header << std::setfill(' ') << "\n";
}

} // namespace pir
} // namespace rir
