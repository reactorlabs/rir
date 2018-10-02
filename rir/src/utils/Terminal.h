#ifndef RIR_TERMINAL_
#define RIR_TERMINAL_

#include <iostream>
#include <string>
#include <unistd.h>

struct ConsoleColor {
    static bool isTTY(std::ostream& out) {
        if (out.rdbuf() == std::cout.rdbuf() && isatty(fileno(stdout))) {
            return true;
        } else if (out.rdbuf() == std::cerr.rdbuf() && isatty(fileno(stderr))) {
            return true;
        }
        return false;
    }
    static void red(std::ostream& out) { out << "\033[1;31m"; }
    static void cyan(std::ostream& out) { out << "\033[1;30m"; }
    static void yellow(std::ostream& out) { out << "\033[1;33m"; }
    static void blue(std::ostream& out) { out << "\033[1;34m"; }
    static void magenta(std::ostream& out) { out << "\033[1;35m"; }
    static void clear(std::ostream& out) { out << "\033[0m"; }
};

#endif
