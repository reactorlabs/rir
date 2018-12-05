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

class StreamLogger;

class LogStream {
  private:
    const std::string id;
    DebugOptions options;
    bool printedAnything = false;

  public:
    LogStream(const LogStream&) = delete;
    LogStream& operator=(const LogStream&) = delete;

    void pirOptimizationsFinished(Closure*);
    void compilationEarlyPir(Closure*);
    void pirOptimizationsHeader(Closure*, const std::string&, size_t);
    void pirOptimizations(Closure*);
    void afterAllocator(Code*, std::function<void(std::ostream&)>);
    void CSSA(Code*);
    void finalPIR(Closure*);
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

    LogStream(const DebugOptions& options, const std::string& id,
              std::ostream& stream = std::cout)
        : id(id), options(options), out(stream) {}
    friend class StreamLogger;
};

class FileLogStream : public LogStream {
  public:
    ~FileLogStream() override;

  private:
    std::ofstream fstream;

  protected:
    bool tty() override { return false; }

    FileLogStream(const DebugOptions& options, const std::string& id,
                  const std::string& fileName)
        : LogStream(options, id, fstream), fstream(fileName) {}
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

    BufferedLogStream(const DebugOptions& options, const std::string& id,
                      std::ostream& actualOut = std::cout)
        : LogStream(options, id, sstream), actualOut(actualOut) {}
    friend class StreamLogger;
};

class StreamLogger {
  public:
    explicit StreamLogger(DebugOptions options) : options(options) {}
    ~StreamLogger();

    static uint64_t logId;

    StreamLogger(const StreamLogger&) = delete;
    StreamLogger& operator=(const StreamLogger&) = delete;

    LogStream& begin(Closure* cls);
    LogStream& get(Closure* cls) {
        assert(streams.count(cls) && "You need to call begin first");
        return *streams.at(cls);
    }

    void warn(const std::string& msg);

    void title(const std::string& msg);
    void flush();

    void close(Closure* cls) { streams.erase(cls); }

  private:
    std::unordered_map<Closure*, std::unique_ptr<LogStream>> streams;
    DebugOptions options;
};

} // namespace pir
} // namespace rir

#endif
