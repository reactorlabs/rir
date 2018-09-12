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
    LogStream(const LogStream&) = delete;
    LogStream& operator=(const LogStream&) = delete;
    const std::string id;
    std::ostream& out;
    DebugOptions options;
    bool printedAnything = false;

  public:
    void pirOptimizationsFinished(Closure*);
    void compilationEarlyPir(Closure*);
    void pirOptimizations(Closure*, const std::string&, size_t);
    void afterAllocator(Code*, std::function<void(std::ostream&)>);
    void CSSA(Code*);
    void finalPIR(Closure*);
    void finalRIR(Function*);
    void unsupportedBC(const std::string&, rir::BC);
    void failed(const std::string& msg);
    void warn(const std::string& msg);

    void section(const std::string&);

    void preparePrint() {
        if (!printedAnything)
            header();
        printedAnything = true;
    }

    virtual void flush() {
        if (printedAnything) {
            footer();
            out.flush();
        }
        printedAnything = false;
    }
    virtual ~LogStream() { assert(!printedAnything && "Forgot to flush"); }

  protected:
    virtual void highlightOn();
    virtual void highlightOff();

    void header();
    void footer();

    LogStream(const DebugOptions& options, const std::string& id,
              std::ostream& stream = std::cout)
        : id(id), out(stream), options(options) {}
    friend class StreamLogger;
};

class FileLogStream : public LogStream {
  public:
    ~FileLogStream() override;

  private:
    std::ofstream fstream;

  protected:
    void highlightOn() override{};
    void highlightOff() override{};

    FileLogStream(const DebugOptions& options, const std::string& id,
                  const std::string& fileName)
        : LogStream(options, id, fstream), fstream(fileName) {}
    friend class StreamLogger;
};

class BufferedLogStream : public LogStream {
  public:
    void flush() override {
        LogStream::flush();
        std::cout << sstream.str();
        sstream.str("");
        std::cout.flush();
    }

  private:
    std::stringstream sstream;

  protected:
    BufferedLogStream(const DebugOptions& options, const std::string& id)
        : LogStream(options, id, sstream) {}
    friend class StreamLogger;
};

class StreamLogger {
  public:
    StreamLogger(DebugOptions options) : options(options) {}
    ~StreamLogger();

    static uint64_t logId;

    StreamLogger(const StreamLogger&) = delete;
    StreamLogger& operator=(const StreamLogger&) = delete;

    LogStream& begin(Closure* cls, const std::string& name);
    LogStream& get(Closure* cls) {
        assert(streams.count(cls) && "You need to call begin first");
        return *streams.at(cls);
    }

    void warn(const std::string& msg);

    void flush();
    void close(Closure* cls) { streams.erase(cls); }

  private:
    std::unordered_map<Closure*, std::unique_ptr<LogStream>> streams;
    DebugOptions options;
};

} // namespace pir
} // namespace rir

#endif
