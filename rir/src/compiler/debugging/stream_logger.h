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
class ClosureStreamLogger;
class StreamLogger;
class LogStream {
  private:
    bool printedAnything = false;

  public:
    std::ostream& out;

    LogStream(const LogStream&) = delete;
    LogStream& operator=(const LogStream&) = delete;

    virtual bool tty() = 0;

    virtual ~LogStream() { assert(!printedAnything && "Forgot to flush"); }

    virtual void flush() {
        if (printedAnything) {
            out.flush();
        }
        printedAnything = false;
    }

    template <typename T>
    friend LogStream& operator<<(LogStream& log, T dump) {
        log.printedAnything = true;
        log.out << dump;
        return log;
    }

    template <typename T>
    void operator()(T* dump) {
        printedAnything = true;
        dump->print(out, tty());
    }
    template <typename T>
    void operator()(T dump) {
        printedAnything = true;
        dump.print(out, tty());
    }

  protected:
    LogStream(std::ostream& out) : out(out) {}
};

class FileLogStream : public LogStream {
    std::ofstream fstream;

  public:
    FileLogStream(const std::string& fileName)
        : LogStream(fstream), fstream(fileName) {}
    ~FileLogStream() override;

    bool tty() override { return false; }
};

class BufferedLogStream : public LogStream {
    std::stringstream sstream;
    std::ostream& actualOut;

  public:
    BufferedLogStream(std::ostream& actualOut = std::cout)
        : LogStream(sstream), actualOut(actualOut) {}

    void flush() override {
        LogStream::flush();
        actualOut << sstream.str();
        sstream.str("");
        actualOut.flush();
    }

    bool tty() override;
};

class SimpleLogStream : public LogStream {
  public:
    SimpleLogStream(std::ostream& out = std::cout) : LogStream(out) {}

    bool tty() override;
};

class PassStreamLogger {
  private:
    const size_t number;
    DebugOptions options;
    ClosureStreamLogger& parent;
    std::shared_ptr<LogStream> _out;

  public:
    LogStream& out() { return *_out; }

    void pirOptimizationsHeader(ClosureVersion*, const PirTranslator*);
    void pirOptimizations(ClosureVersion*, const PirTranslator*);

    void flush() { out().flush(); }

  protected:
    PassStreamLogger(size_t number, DebugOptions options,
                     ClosureStreamLogger& parent,
                     std::shared_ptr<LogStream> out)
        : number(number), options(options), parent(parent), _out(out) {}

    friend class ClosureStreamLogger;
};

class ClosureStreamLogger {
  private:
    bool printedAnything = false;
    const ClosureVersion* version;
    DebugOptions options;
    std::shared_ptr<LogStream> _out;

  public:
    LogStream& out() { return *_out; }
    PassStreamLogger forPass(size_t number);

    void pirOptimizationsFinished(ClosureVersion*);
    void compilationEarlyPir(ClosureVersion*);
    void afterAllocator(Code*, std::function<void(std::ostream&)>);
    void CSSA(Code*);
    void finalPIR(ClosureVersion*);
    void finalRIR(Function*);
    void unsupportedBC(const std::string&, const rir::BC&);
    void failed(const std::string& msg);
    void warn(const std::string& msg);

    void section(const std::string&);

    void preparePrint() {
        if (!printedAnything)
            header();
        printedAnything = true;
    }

    void flush() {
        if (printedAnything) {
            footer();
            out().flush();
        }
        printedAnything = false;
    }

  protected:
    ClosureStreamLogger(const DebugOptions& options,
                        const ClosureVersion* version,
                        std::shared_ptr<LogStream> out)
        : version(version), options(options), _out(out) {}

    void header();
    void footer();
    void highlightOn();
    void highlightOff();

    friend class PassStreamLogger;
    friend class StreamLogger;
};

class StreamLogger {
  public:
    explicit StreamLogger(const DebugOptions& options) : options(options) {}
    ~StreamLogger();

    static uint64_t logId;

    StreamLogger(const StreamLogger&) = delete;
    StreamLogger& operator=(const StreamLogger&) = delete;

    ClosureStreamLogger& begin(ClosureVersion* cls);
    ClosureStreamLogger& get(ClosureVersion* cls) {
        if (!streams.count(cls))
            begin(cls);
        return streams.at(cls);
    }

    void warn(const std::string& msg);

    void title(const std::string& msg);
    void flush();

    void close(ClosureVersion* cls) { streams.erase(cls); }

  private:
    std::unordered_map<ClosureVersion*, ClosureStreamLogger> streams;
    DebugOptions options;
};

} // namespace pir
} // namespace rir

#endif
