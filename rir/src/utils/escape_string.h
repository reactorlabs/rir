#ifndef ESCAPE_STRING
#define ESCAPE_STRING

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <unistd.h>

namespace rir {

static std::string escapeString(std::string str) {
    std::string res = "";
    for (char c : str) {
        if (c >= ' ' && c <= '~' && c != '\\' && c != '"') {
            res += c;
        } else {
            res += '\\';
            switch (c) {
            case '"':
            case '\\':
                res += c;
                break;
            case '\n':
                res += 'n';
                break;
            case '\r':
                res += 'r';
                break;
            case '\t':
                res += 't';
                break;
            default:
                const char* hexs = "0123456789ABCDEF";
                res += 'x';
                res += hexs[c >> 4];
                res += hexs[c & 0xF];
                break;
            }
        }
    }
    return res;
}

} // namespace rir

#endif
