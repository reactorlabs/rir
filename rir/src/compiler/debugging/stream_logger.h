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
    explicit FileLogStream(const std::string& fileName)
        : LogStream(fstream), fstream(fileName) {}
    ~FileLogStream() override;

    bool tty() override { return false; }
};

class BufferedLogStream : public LogStream {
    std::stringstream sstream;
    std::ostream& actualOut;

  public:
    explicit BufferedLogStream(std::ostream& actualOut = std::cout)
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
    explicit SimpleLogStream(std::ostream& out = std::cout) : LogStream(out) {}

    bool tty() override;
};

class GenericStreamLogger {
  protected:
    std::shared_ptr<LogStream> _out;
    const DebugOptions options;
    const ClosureVersion* version;

    GenericStreamLogger(DebugOptions options, const ClosureVersion* version,
                        const std::shared_ptr<LogStream>& out)
        : _out(out), options(options), version(version) {}

    void header();
    void footer();
    void highlightOn();
    void highlightOff();

  public:
    LogStream& out() { return *_out; }

    virtual void preparePrint() = 0;
    virtual void flush() = 0;

    void section(const std::string&);
    void failed(const std::string& msg);
    void warn(const std::string& msg);
};

class PassStreamLogger : public GenericStreamLogger {
    const size_t number;
    ClosureStreamLogger& parent;

  public:
    void pirOptimizationsHeader(const PirTranslator*);
    void pirOptimizations(const PirTranslator*);

    void preparePrint() override;
    void flush() override { out().flush(); }

  protected:
    PassStreamLogger(size_t number, ClosureStreamLogger& parent,
                     const std::shared_ptr<LogStream>& out);

    friend class ClosureStreamLogger;
};

class ClosureStreamLogger : public GenericStreamLogger {
  private:
    bool printedAnything = false;
    const size_t logId;

  public:
    PassStreamLogger forPass(size_t number);

    void pirOptimizationsFinished(ClosureVersion*);
    void compilationEarlyPir(ClosureVersion*);
    void afterAllocator(Code*, std::function<void(std::ostream&)>);
    void CSSA(Code*);
    void finalPIR(ClosureVersion*);
    void finalRIR(Function*);
    void unsupportedBC(const std::string&, const rir::BC&);

    void preparePrint() override {
        if (!printedAnything)
            header();
        printedAnything = true;
    }

    void flush() override {
        if (printedAnything) {
            footer();
            out().flush();
        }
        printedAnything = false;
    }

  protected:
    ClosureStreamLogger(const DebugOptions& options, const size_t logId,
                        const ClosureVersion* version,
                        const std::shared_ptr<LogStream>& out)
        : GenericStreamLogger(options, version, out), logId(logId) {}

    friend class PassStreamLogger;
    friend class StreamLogger;
};

class StreamLogger {
    static size_t logId;

  public:
    explicit StreamLogger(const DebugOptions& options) : options(options) {}
    ~StreamLogger();

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
