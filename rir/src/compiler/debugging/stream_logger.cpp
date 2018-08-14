#include "stream_logger.h"
#include "../../utils/Pool.h"
#include <iomanip>

namespace rir {
namespace pir {

void StreamLogger::startLogging(rir::Function* function) {
    if (options.intersects(PrintDebugPasses)) {
        if (!streams.count(function)) {
            streams.insert(std::pair<rir::Function*, std::stringstream*>(
                function, new std::stringstream()));
        }
        currentFunction.push(function);
    }
}

void StreamLogger::endLogging() {
    if (options.intersects(PrintDebugPasses))
        currentFunction.pop();
}

void StreamLogger::compilationInit() {
    if (options.intersects(PrintDebugPasses)) {
        getLog() << "\n***********************************************"
                 << "***************\n";
        getLog() << "*********** Start compiling: " << std::setw(20)
                 << std::left << currentFunction.top() << " ************\n\n";
        if (options.includes(DebugFlag::PrintEarlyRir)) {
            getLog() << "=============== Original version:" << std::setw(16)
                     << std::left << " ==========\n\n";
            auto it = currentFunction.top()->begin();
            while (it != currentFunction.top()->end()) {
                (*it)->print(getLog());
                ++it;
            }
            getLog() << "\n";
        }
    }
}

void StreamLogger::compilationEarlyPir(Closure& closure) {
    if (options.includes(DebugFlag::PrintEarlyPir)) {
        getLog() << "\n========= Compiled to PIR Version" << std::setw(16)
                 << std::left << " ==========\n";
        closure.print(getLog());
    }
}

void StreamLogger::pirOptimizations(Closure& closure,
                                    const std::string& category,
                                    const std::string& pass, size_t passnr) {
    getLog() << "============== " << category << ": " << pass
             << " == " << passnr << std::setw(10) << std::left
             << " ======================\n";
    closure.print(getLog());
}

void StreamLogger::phiInsertion(Closure& closure) {
    getLog() << "========= After phi copies inserted" << std::setw(14)
             << std::left << " ==========\n";
    closure.print(getLog());
}

void StreamLogger::rirFromPir(rir::Function* function) {
    if (options.includes(DebugFlag::PrintFinalRir)) {
        getLog() << "\n============= Final RIR Version" << std::setw(16)
                 << std::left << " ==========\n";
        auto it = function->begin();
        while (it != function->end()) {
            (*it)->print(getLog());
            ++it;
        }
    }
}

void StreamLogger::afterCSSA(Code* code) {
    if (options.includes(DebugFlag::PrintCSSA)) {
        getLog() << " ========== PIR After Converting to CSSA\n";
        code->print(getLog());
    }
}

/*void StreamLogger::afterLiveness(SSAAllocator& allocator) {
    if (options.includes(DebugFlag::PrintLivenessIntervals))
            allocator.print(getLog());
}*/

void StreamLogger::finalPIR(Code* code) {
    if (options.includes(DebugFlag::PrintFinalPir)) {
        getLog() << "\n ========== Final PIR Version\n";
        code->print(getLog());
    }
}

void StreamLogger::finish(rir::Function* function, std::stringstream& stream) {
    if (options.intersects(PrintDebugPasses)) {
        stream << "\n*********** Finished compiling: " << std::setw(17)
               << std::left << function << " ************\n";
        stream << "*************************************************"
               << "*************\n";
    }
}

void StreamLogger::warningBC(std::string warning, rir::BC bc) {
    if (options.includes(DebugFlag::ShowWarnings)) {
        getLog() << "Warning" << warning << ": ";
        if (warning.compare("Guard ignored") == 0) {
            CHAR(PRINTNAME(rir::Pool::get(bc.immediate.guard_fun_args.name)));
        } else {
            bc.print(getLog());
        }
        getLog() << "\n";
    }
}

void StreamLogger::failCompilingPir() {
    if (options.includes(DebugFlag::ShowWarnings))
        getLog() << " Failed verification after p2r compile "
                 << &currentFunction.top() << "\n";
}

} // namespace pir
} // namespace rir