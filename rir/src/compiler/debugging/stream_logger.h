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

class ClosureStreamLogger {
  private:
    const ClosureVersion* version;
    DebugOptions options;
    bool printedAnything = false;

  public:
    ClosureStreamLogger(const ClosureStreamLogger&) = delete;
    ClosureStreamLogger& operator=(const ClosureStreamLogger&) = delete;

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

    virtual ~ClosureStreamLogger() {
        assert(!printedAnything && "Forgot to flush");
    }

    virtual void flush() {
        if (printedAnything) {
            footer();
            out.flush();
        }
        printedAnything = false;
    }

    template <typename T>
    friend ClosureStreamLogger& operator<<(ClosureStreamLogger& log, T dump) {
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

    ClosureStreamLogger(const DebugOptions& options,
                        const ClosureVersion* version,
                        std::ostream& stream = std::cout)
        : version(version), options(options), out(stream) {}
    friend class StreamLogger;
};

class FileLogStream : public ClosureStreamLogger {
  public:
    ~FileLogStream() override;

  private:
    std::ofstream fstream;

  protected:
    bool tty() override { return false; }

    FileLogStream(const DebugOptions& options, const ClosureVersion* version,
                  const std::string& fileName)
        : ClosureStreamLogger(options, version, fstream), fstream(fileName) {}
    friend class StreamLogger;
};

class BufferedLogStream : public ClosureStreamLogger {
  public:
    void flush() override {
        ClosureStreamLogger::flush();
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
        : ClosureStreamLogger(options, version, sstream), actualOut(actualOut) {
    }
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
        return *streams.at(cls);
    }

    void warn(const std::string& msg);

    void title(const std::string& msg);
    void flush();

    void close(ClosureVersion* cls) { streams.erase(cls); }

  private:
    std::unordered_map<ClosureVersion*, std::unique_ptr<ClosureStreamLogger>>
        streams;
    DebugOptions options;
};

} // namespace pir
} // namespace rir

#endif
