#include "stream_logger.h"
#include "../pir/pir_impl.h"
#include "runtime/Function.h"
#include "utils/Pool.h"

#include <iomanip>

namespace rir {
namespace pir {

void StreamLogger::startLogging(rir::Function* function) {
    if (options.intersects(PrintDebugPasses)) {
        if (!streams.count(function)) {
            std::ostream* o;
            if (options.includes(DebugFlag::PrintIntoFiles)) {
                o = new std::ofstream();
                // TODO: shouldn't the function identifier be part of the name?
                ((std::ofstream*)o)->open(".pir/pirCompilation.data");
            } else if (options.includes(DebugFlag::PrintIntoStdout)) {
                o = &std::cout;
            } else {
                o = new std::stringstream();
            }
            streams.emplace(function, o);
            currentFunction.push(function);
        }
    }
}

void StreamLogger::endLogging() {
    if (options.intersects(PrintDebugPasses))
        currentFunction.pop();
}

void StreamLogger::compilationInit() const {
    if (options.intersects(PrintDebugPasses)) {
        this->header("Start compiling:", currentFunction.top(), getLog(), true);
        if (options.includes(DebugFlag::PrintEarlyRir)) {
            innerHeader(" Original version ");
            for (auto code : *(currentFunction.top())) {
                code->print(getLog());
            }
            getLog() << "\n";
        }
    }
}

void StreamLogger::compilationEarlyPir(const Closure& closure) const {
    if (options.includes(DebugFlag::PrintEarlyPir)) {
        innerHeader(" Compiled to PIR Version ");
        closure.print(getLog());
    }
}

void StreamLogger::pirOptimizations(const Closure& closure,
                                    const std::string& category,
                                    const std::string& pass,
                                    size_t passnr) const {
    if (options.includes(DebugFlag::PrintOptimizationPasses)) {
        std::stringstream ss;
        ss << category << ": " << pass << " == " << passnr;
        innerHeader(ss.str());
        closure.print(getLog());
    }
}

void StreamLogger::rirFromPir(rir::Function* function) const {
    if (options.includes(DebugFlag::PrintFinalRir)) {
        innerHeader(" Final RIR Version ");
        for (auto code : *function) {
            code->print(getLog());
        }
        getLog() << "\n";
    }
}

void StreamLogger::phiInsertion(const Code* code) const {
    if (options.includes(DebugFlag::PrintCSSA)) {
        innerHeader(" After Phi Copies Inserted ");
        code->print(getLog());
    }
}

void StreamLogger::afterCSSA(const Code* code) const {
    if (options.includes(DebugFlag::PrintCSSA)) {
        innerHeader(" PIR After Converting to CSSA ");
        code->print(getLog());
    }
}

/*void StreamLogger::afterLiveness(SSAAllocator& allocator) {
    if (options.includes(DebugFlag::PrintLivenessIntervals))
            allocator.print(getLog());
}*/

void StreamLogger::finalPIR(const Code* code) const {
    if (options.includes(DebugFlag::PrintFinalPir)) {
        innerHeader(" Final PIR Version ");
        code->print(getLog());
    }
}

void StreamLogger::finish(const rir::Function* function,
                          std::ostream& stream) const {
    if (options.intersects(PrintDebugPasses)) {
        header("Finished compiling:", function, stream, false);
    }
}

void StreamLogger::warningBC(std::string warning, rir::BC bc) const {
    if (options.includes(DebugFlag::ShowWarnings)) {
        getLog() << "Warning" << warning << ": ";
        if (warning.compare(WARNING_GUARD_STRING) == 0) {
            CHAR(PRINTNAME(rir::Pool::get(bc.immediate.guard_fun_args.name)));
        } else {
            bc.print(getLog());
        }
        getLog() << "\n";
    }
}

void StreamLogger::failCompilingPir() const {
    if (options.includes(DebugFlag::ShowWarnings))
        getLog() << " Failed verification after p2r compile "
                 << &currentFunction.top() << "\n";
}

void StreamLogger::header(std::string header, const rir::Function* function,
                          std::ostream& stream, bool opening) const {
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

void StreamLogger::innerHeader(std::string header) const {
    getLog() << "\n"
             << std::setfill('=') << std::setw(15) << "" << std::left
             << std::setw(39) << header << std::setfill(' ') << "\n";
}
} // namespace pir
} // namespace rir
