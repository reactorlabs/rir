#ifndef PIR_LOGGER_H
#define PIR_LOGGER_H

#include "../../runtime/Function.h"
#include "../pir/pir_impl.h"
#include "debugging.h"
//#include "../translations/pir_2_rir.h"

#include <iostream>
#include <map>
#include <sstream>
#include <stack>
//#include <fstream>

namespace rir {
namespace pir {

class StreamLogger {
  public:
    StreamLogger(DebugOptions options) : options(options) {}
    ~StreamLogger() {
        for (std::map<rir::Function*, std::stringstream*>::iterator it =
                 streams.begin();
             it != streams.end(); ++it) {
            this->finish(it->first, *streams.at(it->first));
            /*if (options.includes(DebugFlag::PrintIntoFiles)) {
                std::ofstream outFile;
                outFile.open(".pir/pirCompilation.data");
                outFile << stream.str();
                outFile.close();
            } else {*/
            std::cout << it->second->str();
            //}
            delete it->second;
        }
    }

    void startLogging(rir::Function* function);
    void endLogging();
    void compilationInit();
    void compilationEarlyPir(Closure&);
    void pirOptimizations(Closure&, const std::string&, const std::string&,
                          size_t);
    void phiInsertion(Closure& closure);
    void afterCSSA(Code*);
    // void afterLiveness(SSAAllocator&);
    void finalPIR(Code*);
    void rirFromPir(rir::Function* function);
    void warningBC(std::string, rir::BC bc);
    void failCompilingPir();
    void finish(rir::Function*, std::stringstream&);

    std::stringstream& getLog() { return *streams.at(currentFunction.top()); };

  private:
    std::map<rir::Function*, std::stringstream*> streams;
    const DebugOptions options;
    std::stack<rir::Function*> currentFunction;
};
} // namespace pir
} // namespace rir

#endif