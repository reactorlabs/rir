#include "common.h"
#include "R/RList.h"
#include "R/r.h"

#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"

#include <cxxabi.h>
#include <execinfo.h>
#include <iostream>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

#undef assert

void printCBacktrace() {
    std::cerr << "\nC Backtrace\n\n";
    void* array[128];

    // get void*'s for all entries on the stack
    // see `man backtrace` for details
    int size = backtrace(array, 128);

    char** messages = backtrace_symbols(array, size);

    /* skip first stack frame (points here) */
    for (int i = 1; i < size && messages != NULL; ++i) {
        // The entries of the backtrace contain the mangled C++ symbols in
        // parens. This part extracts the name and demangles it.
        std::string msg = messages[i];
        auto namestart = msg.find("(");
        auto nameend = msg.find(")");
        if (namestart == std::string::npos || nameend == std::string::npos) {
            std::cerr << msg << "\n";
        } else {
            auto name = msg.substr(namestart + 1, nameend - namestart - 1);
            auto plus = name.rfind("+");
            auto offset = name.substr(plus + 1);
            name = name.substr(0, plus);
            int status;
            auto realname = abi::__cxa_demangle(name.c_str(), 0, 0, &status);
            std::cerr << realname << "+" << offset << "\n";
            free(realname);
        }
    }

    free(messages);
}
