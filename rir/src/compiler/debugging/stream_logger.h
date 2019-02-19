#ifndef PIR_LOGGER_H
#define PIR_LOGGER_H

#include "../pir/pir.h"
#include "debugging.h"

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

struct Function;
class BC;

namespace pir {

class PirTranslator;
class StreamLogger;

class LogStream {
  private:
    const ClosureVersion* version;
    DebugOptions options;
    bool printedAnything = false;

  public:
    LogStream(const LogStream&) = delete;
    LogStream& operator=(const LogStream&) = delete;

    void pirOptimizationsFinished(ClosureVersion*);
    void compilationEarlyPir(ClosureVersion*);
    void pirOptimizationsHeader(ClosureVersion*, const PirTranslator*, size_t);
    void pirOptimizations(ClosureVersion*, const PirTranslator*);
    void afterAllocator(Code*, std::function<void(std::ostream&)>);
    void CSSA(Code*);
    void finalPIR(ClosureVersion*);
    void finalRIR(Function*);
    void unsupportedBC(const std::string&, const rir::BC&);
    void failed(const std::string& msg);
    void warn(const std::string& msg);

    void section(const std::string&);
    virtual bool tty();

    void preparePrint() {
        if (!printedAnything)
            header();
        printedAnything = true;
    }

    virtual ~LogStream() { assert(!printedAnything && "Forgot to flush"); }

    virtual void flush() {
        if (printedAnything) {
            footer();
            out.flush();
        }
        printedAnything = false;
    }

    template <typename T>
    friend LogStream& operator<<(LogStream& log, T dump) {
        log.out << dump;
        return log;
    }

    template <typename T>
    void operator()(T* dump) {
        dump->print(out, tty());
    }
    template <typename T>
    void operator()(T dump) {
        dump.print(out, tty());
    }

  protected:
    virtual void highlightOn();
    virtual void highlightOff();

    std::ostream& out;
    void header();
    void footer();

    LogStream(const DebugOptions& options, const ClosureVersion* version,
              std::ostream& stream = std::cout)
        : version(version), options(options), out(stream) {}
    friend class StreamLogger;
};

class FileLogStream : public LogStream {
  public:
    ~FileLogStream() override;

  private:
    std::ofstream fstream;

  protected:
    bool tty() override { return false; }

    FileLogStream(const DebugOptions& options, const ClosureVersion* version,
                  const std::string& fileName)
        : LogStream(options, version, fstream), fstream(fileName) {}
    friend class StreamLogger;
};

class BufferedLogStream : public LogStream {
  public:
    void flush() override {
        LogStream::flush();
        actualOut << sstream.str();
        sstream.str("");
        actualOut.flush();
    }

  private:
    std::stringstream sstream;
    std::ostream& actualOut;

  protected:
    bool tty() override;

    BufferedLogStream(const DebugOptions& options, ClosureVersion* version,
                      std::ostream& actualOut = std::cout)
        : LogStream(options, version, sstream), actualOut(actualOut) {}
    friend class StreamLogger;
};

class StreamLogger {
  public:
    explicit StreamLogger(const DebugOptions& options) : options(options) {}
    ~StreamLogger();

    static uint64_t logId;

    StreamLogger(const StreamLogger&) = delete;
    StreamLogger& operator=(const StreamLogger&) = delete;

    LogStream& begin(ClosureVersion* cls);
    LogStream& get(ClosureVersion* cls) {
        if (!streams.count(cls))
            begin(cls);
        return *streams.at(cls);
    }

    void warn(const std::string& msg);

    void title(const std::string& msg);
    void flush();

    void close(ClosureVersion* cls) { streams.erase(cls); }

  private:
    std::unordered_map<ClosureVersion*, std::unique_ptr<LogStream>> streams;
    DebugOptions options;
};

} // namespace pir
} // namespace rir

#endif
