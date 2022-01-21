#ifndef PIR_LOG_SINKS_H
#define PIR_LOG_SINKS_H

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

struct Function;
class BC;

namespace pir {
class Pass;

class LogStream {
    /* LogStream Interface and abstract class */
  private:
    bool printedAnything = false;

  public:
    virtual std::ostream& out() = 0;

    LogStream(const LogStream&) = delete;
    LogStream& operator=(const LogStream&) = delete;

    virtual bool tty() = 0;

    virtual ~LogStream() { assert(!printedAnything && "Forgot to flush"); }

    virtual void flush() {
        if (printedAnything) {
            out().flush();
        }
        printedAnything = false;
    }

    template <typename T>
    friend LogStream& operator<<(LogStream& log, T dump) {
        log.printedAnything = true;
        log.out() << dump;
        return log;
    }

    template <typename T>
    void operator()(T* dump) {
        printedAnything = true;
        dump->print(out(), tty());
    }
    template <typename T>
    void operator()(T dump) {
        printedAnything = true;
        dump.print(out(), tty());
    }

    virtual std::string comment() { return ""; }

  protected:
    LogStream() {}
};

class FileLogStream : public LogStream {
    std::unique_ptr<std::ofstream> fstream_;
    std::ofstream& fstream();

  public:
    explicit FileLogStream(const std::string& fileName) : fileName(fileName) {}
    ~FileLogStream() override;

    virtual std::ostream& out() override final;
    bool tty() override { return false; }

  protected:
    const std::string fileName;
    bool closeFile();
};

class DotFileLogStream : public FileLogStream {
  public:
    DotFileLogStream(const std::string& fileName) : FileLogStream(fileName) {}

    ~DotFileLogStream() override;

    std::string comment() override final { return "//"; }
};

class BufferedLogStream : public LogStream {
    std::stringstream sstream;
    std::ostream& actualOut;

  public:
    explicit BufferedLogStream(std::ostream& actualOut = std::cout)
        : actualOut(actualOut) {}
    ~BufferedLogStream() override;

    virtual std::ostream& out() override final { return sstream; }

    void flush() override {
        LogStream::flush();
        actualOut << sstream.str();
        sstream.str("");
        actualOut.flush();
    }

    bool tty() override;
};

class UnbufferedLogStream : public LogStream {
  public:
    virtual std::ostream& out() override final { return std::cout; }

    bool tty() override;
};

} // namespace pir
} // namespace rir

#endif
