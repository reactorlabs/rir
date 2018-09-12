#ifndef PIR_LOGGER_H
#define PIR_LOGGER_H

#include "../pir/pir.h"
#include "debugging.h"

#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <ostream>
#include <sstream>
#include <stack>

namespace rir {

struct Function;
class BC;

namespace pir {

class StreamLogger {
    static uint64_t logId;

  public:
    StreamLogger(DebugOptions options) : options(options) {}
    ~StreamLogger();

    StreamLogger(const StreamLogger&) = delete;
    StreamLogger& operator=(const StreamLogger&) = delete;

    void compilationEarlyPir(Closure&);
    void pirOptimizations(Closure&, const std::string&, size_t);
    void pirOptimizationsFinished(Closure&);
    void afterCSSA(Closure&, const Code*);
    void afterAllocator(Closure&, std::function<void(std::ostream&)>);
    void finalPIR(Closure&);
    void rirFromPir(rir::Function*);
    void warningBC(rir::Function*, std::string, rir::BC);
    void failCompilingPir(rir::Function*);
    void header(std::string, const rir::Function*, std::ostream&, bool);
    void innerHeader(rir::Function*, std::string);

    std::ostream& getLog(rir::Function* function) {
        if (!streams.count(function))
            startLogging(function);
        return *streams.at(function);
    };

  private:
    std::map<rir::Function*, std::unique_ptr<std::stringstream>> streams;
    const DebugOptions options;

    void startLogging(rir::Function* function);
    void compilationInit(rir::Function*);
    void finish(const rir::Function*, std::ostream&);
};

} // namespace pir
} // namespace rir

#endif
