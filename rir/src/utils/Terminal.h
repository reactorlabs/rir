#ifndef RIR_TERMINAL_
#define RIR_TERMINAL_

#include <iostream>
#include <utility>
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
    static void yellow(std::ostream& out) { out << "\033[1;33m"; }
    static void blue(std::ostream& out) { out << "\033[1;34m"; }
    static void magenta(std::ostream& out) { out << "\033[1;35m"; }
    static void clear(std::ostream& out) { out << "\033[0m"; }
};

namespace console {
    template<void (*color)(std::ostream&)> struct SetColor {
        friend std::ostream& operator<<(std::ostream& out, const SetColor<color>& r) {
            color(out);
            return out;
        }
    };
    template<void (*color)(std::ostream&)> struct WithColor {
        std::string msg;
        explicit WithColor(std::string msg) : msg(std::move(msg)) {}

        friend std::ostream& operator<<(std::ostream& out, const WithColor<color>& r) {
            out << SetColor<color>{} << r.msg << SetColor<ConsoleColor::clear>{};
            return out;
        }
    };
    __attribute__((unused)) static const SetColor<ConsoleColor::clear> clear{};
    __attribute__((unused)) static WithColor<ConsoleColor::red> with_red(std::string msg) {
        return WithColor<ConsoleColor::red>(std::move(msg));
    }
} // namespace console

#endif
