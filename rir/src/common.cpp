#include "common.h"
#include "R/RList.h"
#include "R/r.h"

#include "interpreter/LazyArglist.h"
#include "interpreter/LazyEnvironment.h"
#include "utils/capture_out.h"

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

// Helper method that iterates over the current stack of R contentxts and prints
// some information for each of them, like the environment, the ast of the call,
// the promargs and so on.
// This is very useful for debugging. For example to call it from GDB, or if you
// have an error in some test, but fail to reproduce outside the testrunner, you
// can add explicitly call this backtrace method, to see where exactly you are.
void printRBacktrace() {
    std::cerr << "\nR Backtrace\n\n";

    RCNTXT* ctx = (RCNTXT*)R_GlobalContext;

    auto toString = [&](SEXP v, size_t maxLen = 32) {
        rir::CaptureOut c;
        Rf_PrintValue(v);
        auto r = c();
        r.pop_back();
        std::string::size_type pos = 0;
        while ((pos = r.find("\n", pos)) != std::string::npos) {
            r[pos] = ' ';
        }
        if (r.length() > maxLen)
            r = r.substr(0, maxLen - 2) + "..";
        return r;
    };

    while (ctx) {
        std::cerr << "* Context " << ctx;

        if (rir::LazyArglist::check(ctx->promargs)) {
            std::cerr << ", (lazy promargs)\n";
        } else {
            std::cerr << "\n  ARGS: ";
            auto args = rir::RList(ctx->promargs);
            for (auto a = args.begin(); a != args.end(); ++a) {
                if (a.hasTag())
                    std::cerr << CHAR(PRINTNAME(a.tag())) << "=";
                auto arg = *a;

                if (TYPEOF(arg) == PROMSXP && PRVALUE(arg) != R_UnboundValue) {
                    arg = PRVALUE(arg);
                }
                if (TYPEOF(arg) == PROMSXP) {
                    std::cerr << arg;
                } else {
                    std::cerr << "'" << toString(arg) << "'";
                }
                if (a + 1 != args.end())
                    std::cerr << ", ";
            }
        }

        std::cerr << "\n  ENV: ";
        auto args = rir::RList(FRAME(ctx->cloenv));
        for (auto a = args.begin(); a != args.end(); ++a) {
            if (a.hasTag())
                std::cerr << CHAR(PRINTNAME(a.tag())) << "=";
            auto arg = *a;

            if (TYPEOF(arg) == PROMSXP && PRVALUE(arg) != R_UnboundValue) {
                arg = PRVALUE(arg);
            }
            if (TYPEOF(arg) == PROMSXP) {
                std::cerr << arg;
            } else {
                std::cerr << "'" << toString(arg) << "'";
            }
            if (a + 1 != args.end())
                std::cerr << ", ";
        }

        std::cerr << "\n  CLOS: " << toString(ctx->callfun, 100);
        std::cerr << "\n  AST: " << toString(ctx->call, 100) << "\n";

        ctx = ctx->nextcontext;
    }
}

void printBacktrace() {
    printCBacktrace();
    printRBacktrace();
}
