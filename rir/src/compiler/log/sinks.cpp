#include "sinks.h"

#include "compiler/opt/pass.h"
#include "compiler/pir/closure.h"
#include "compiler/pir/closure_version.h"
#include "runtime/Function.h"
#include "utils/Pool.h"
#include "utils/Terminal.h"
#include "utils/filesystem.h"

#include <fstream>
#include <iomanip>
#include <memory>
#include <sstream>
#include <sys/stat.h>

#include <stdlib.h>

namespace rir {
namespace pir {

std::ostream& FileLogStream::out() {
    if (fstream_.get())
        return *fstream_;
    fstream_.reset(new std::ofstream(fileName));
    if (!fstream_->good()) {
        std::cerr << "Warning: FileLogStream(" << fileName
                  << ") error: " << strerror(errno) << "\n";
    }
    return out();
}

bool FileLogStream::closeFile() {
    if (fstream_.get() && fstream_->is_open()) {
        fstream_->flush();
        fstream_->close();
        return true;
    }
    return false;
}

FileLogStream::~FileLogStream() {
    flush();
    closeFile();
}

static void ignore(int m) {}
DotFileLogStream::~DotFileLogStream() {
    flush();
    if (closeFile())
        ignore(system(("dot -Tpdf -O '" + fileName + "'&").c_str()));
}

BufferedLogStream::~BufferedLogStream() { BufferedLogStream::flush(); }

bool BufferedLogStream::tty() { return ConsoleColor::isTTY(actualOut); }
bool UnbufferedLogStream::tty() { return ConsoleColor::isTTY(out()); }

} // namespace pir
} // namespace rir
