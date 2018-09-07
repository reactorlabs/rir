#ifndef PIR_LOGGER_H
#define PIR_LOGGER_H

#include "../pir/pir.h"
#include "debugging.h"

#include <fstream>
#include <iostream>
#include <map>
#include <ostream>
#include <sstream>
#include <stack>

namespace rir {

struct Function;
class BC;

namespace pir {

class StreamLogger {
  public:
    StreamLogger(DebugOptions options) : options(options) {}
    ~StreamLogger() {
        for (auto fun2stream : streams) {
            finish(fun2stream.first, *streams.at(fun2stream.first));
            if (options.includes(DebugFlag::PrintIntoStdout))
                continue;
            else {
                if (options.includes(DebugFlag::PrintIntoFiles)) {
                    ((std::ofstream*)fun2stream.second)->close();
                } else
                    std::cout << ((std::stringstream*)fun2stream.second)->str();
                delete fun2stream.second;
            }
        }
    }

    StreamLogger(const StreamLogger&) = delete;
    StreamLogger& operator=(StreamLogger other) = delete;

    void compilationEarlyPir(Closure&);
    void pirOptimizations(Closure&, const std::string&, size_t);
    void afterCSSA(Closure&, const Code*);
    // void afterLiveness(SSAAllocator&);
    void finalPIR(Closure&);
    void rirFromPir(rir::Function*);
    void warningBC(rir::Function*, std::string, rir::BC);
    void failCompilingPir(rir::Function*);
    void header(std::string, const rir::Function*, std::ostream&, bool);
    void innerHeader(rir::Function*, std::string);

    std::ostream& getLog(rir::Function* function) {
        return std::cout;
        if (!streams.count(function))
            startLogging(function);
        return *streams.at(function);
    };

  private:
    std::map<rir::Function*, std::ostream*> streams;
    const DebugOptions options;

    void startLogging(rir::Function* function);
    void compilationInit(rir::Function*);
    void finish(const rir::Function*, std::ostream&);
};
} // namespace pir
} // namespace rir

#endif
