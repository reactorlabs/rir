#ifndef PIR_LOGGER_H
#define PIR_LOGGER_H

#include "../../runtime/Function.h"
#include "../pir/pir_impl.h"
#include "debugging.h"

#include <iostream>
#include <map>
//#include <fstream>
#include <sstream>
#include <stack>

namespace rir {
namespace pir {

class StreamLogger {
  public:
    StreamLogger(DebugOptions options) : options(options) {}
    ~StreamLogger() {
        for (auto fun2stream : streams) {
            finish(fun2stream.first, *streams.at(fun2stream.first));
            if (options.includes(DebugFlag::PrintIntoFiles)) {
                // std::ofstream outFile;
                // outFile.open(".pir/pirCompilation.data");
                // outFile << stream.str();
                // outFile.close();
            } else
                std::cout << fun2stream.second->str();
            delete fun2stream.second;
        }
    }

    StreamLogger(const StreamLogger&) = delete;
    StreamLogger& operator=(StreamLogger other) = delete;

    void startLogging(rir::Function* function);
    void endLogging();
    void compilationInit() const;
    void compilationEarlyPir(Closure&) const;
    void pirOptimizations(const Closure&, const std::string&,
                          const std::string&, size_t) const;
    void phiInsertion(Code*) const;
    void afterCSSA(Code*) const;
    // void afterLiveness(SSAAllocator&);
    void finalPIR(Code*) const;
    void rirFromPir(rir::Function* function) const;
    void warningBC(std::string, rir::BC bc) const;
    void failCompilingPir() const;
    void header(std::string, rir::Function*, std::stringstream&, bool) const;
    void innerHeader(std::string) const;
    void finish(rir::Function*, std::stringstream&) const;

    std::stringstream& getLog() const {
        return *streams.at(currentFunction.top());
    };

  private:
    std::map<rir::Function*, std::stringstream*> streams;
    const DebugOptions options;
    std::stack<rir::Function*> currentFunction;
};
} // namespace pir
} // namespace rir

#endif