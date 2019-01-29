#include "common.h"
#include "R/RList.h"
#include "R/r.h"
#include "runtime/ArgsLazyData.h"
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
    int size = backtrace(array, 128);

    char** messages = backtrace_symbols(array, size);

    /* skip first stack frame (points here) */
    for (int i = 1; i < size && messages != NULL; ++i) {
        std::string msg = messages[i];
        auto namestart = msg.find("(");
        auto nameend = msg.find(")");
        if (namestart == std::string::npos || nameend == std::string::npos) {
            std::cerr << msg << "\n";
        } else {
            auto name = msg.substr(namestart + 1, nameend - namestart - 2);
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

void printRBacktrace() {
    std::cerr << "\nR Backtrace\n\n";

    RCNTXT* ctx = R_GlobalContext;

    auto cleanupToS = [&](SEXP v, size_t l = 32) {
        rir::CaptureOut c;
        Rf_PrintValue(v);
        auto r = c();
        r.pop_back();
        std::string::size_type pos = 0;
        while ((pos = r.find("\n", pos)) != std::string::npos) {
            r[pos] = ' ';
        }
        if (r.length() > l)
            r = r.substr(0, l - 2) + "..";
        return r;
    };

    while (ctx) {
        std::cerr << "* Context " << ctx;

        if (rir::ArgsLazyData::check(ctx->promargs)) {
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
                    std::cerr << "'" << cleanupToS(arg) << "'";
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
                std::cerr << "'" << cleanupToS(arg) << "'";
            }
            if (a + 1 != args.end())
                std::cerr << ", ";
        }

        std::cerr << "\n  CLOS: " << cleanupToS(ctx->callfun);
        std::cerr << "\n  AST: " << cleanupToS(ctx->call) << "\n";

        ctx = ctx->nextcontext;
    }
}

void printBacktrace() {
    printCBacktrace();
    printRBacktrace();
}
