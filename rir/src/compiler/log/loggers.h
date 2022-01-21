#ifndef PIR_LOGGERS_H
#define PIR_LOGGERS_H

#include "compiler/pir/pir.h"
#include "debug.h"

#include <fstream>
#include <functional>
#include <iostream>
#include <memory>
#include <ostream>
#include <sstream>
#include <stack>
#include <unordered_map>
#include <unordered_set>

namespace rir {
class BC;
namespace pir {

class Pass;

class AbstractLog {
  protected:
    std::shared_ptr<LogStream> _out;
    const DebugOptions options;
    const ClosureVersion* version;

    AbstractLog(DebugOptions options, const ClosureVersion* version,
                std::shared_ptr<LogStream> out)
        : _out(out), options(options), version(version) {}

    void header();
    void footer();
    void highlightOn();
    void highlightOff();

  public:
    LogStream& out() {
        assert(_out);
        return *_out;
    }

    virtual void preparePrint() = 0;
    virtual void flush();

    void section(const std::string&);
    void failed(const std::string& msg);
    void warn(const std::string& msg);
};

class ClosureLog;
class PassLog : public AbstractLog {
    /* Keeps everything related to one pass in one logging unit, ie., one
     * section of a log file, or a dedicated log file per pass, depending on the
     * logging configuration.
     */

    const size_t number;
    ClosureLog& parent;

  public:
    void pirOptimizationsHeader(const Pass*);
    void pirOptimizations(const Pass*);

    void pirOptimizationsFinished();
    void compilationEarlyPir();
    void finalPIR();

    void preparePrint() override;

  protected:
    PassLog(size_t number, ClosureLog& parent, std::shared_ptr<LogStream> out);

    friend class ClosureLog;
};

class ClosureLog : public AbstractLog {
    /* Keeps everything related to one ClosureVersion in one logging unit. */
  private:
    bool printedAnything = false;
    const size_t logId;

  public:
    PassLog forPass(size_t number, const std::string& kind);

    using LLVMBitcodePrint = std::function<void(std::ostream&, bool)>;

    // These are delegated to the pass logger, since e.g., for producing dot
    // files, they must go into their own files.
    void pirOptimizationsFinished() {
        auto log = forPass(1000, "afterOpt");
        log.pirOptimizationsFinished();
        log.flush();
    }
    void finalPIR() {
        auto log = forPass(1001, "final");
        log.finalPIR();
        log.flush();
    }

    void CSSA(Code*);
    void LLVMBitcode(const LLVMBitcodePrint&);
    void unsupportedBC(const std::string&, const rir::BC&);

    void preparePrint() override {
        if (!printedAnything)
            header();
        printedAnything = true;
    }

    void flush() override {
        if (printedAnything) {
            footer();
            AbstractLog::flush();
        }
        printedAnything = false;
    }

    ClosureLog(const DebugOptions& options, const size_t logId,
               const ClosureVersion* version, std::shared_ptr<LogStream> out,
               const std::string& basePath)
        : AbstractLog(options, version, out), logId(logId), basePath(basePath) {
    }

  protected:
    const std::string basePath;

    friend class PassLog;
};

class NullLog : public AbstractLog {};

} // namespace pir
} // namespace rir

#endif
