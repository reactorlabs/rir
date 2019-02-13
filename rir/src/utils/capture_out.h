#ifndef CAPTURE_OUT_H
#define CAPTURE_OUT_H

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <unistd.h>

namespace rir {

// Can be used to capture stdout to a string. Usage:
//
//      std::string output;
//      {
//          CaptureOut capture;
//          doSomeStuffThatWritesToStdout();
//          output = capture();
//      }
//      std::cout << "Captured " << output << "\n";
//
class CaptureOut {
    const static unsigned MAX_LEN = 1024 * 8;
    char buffer[MAX_LEN + 1];
    int out_pipe[2];
    int saved_stdout;

  public:
    CaptureOut() {
        memset(buffer, 0, sizeof(buffer));
        fflush(stdout);
        saved_stdout = dup(STDOUT_FILENO);
        int err = pipe(out_pipe);
        if (err != 0) {
            printf("Pipe err! %s\n", strerror(errno));
            return;
        }
        dup2(out_pipe[1], STDOUT_FILENO);
    }

    std::string operator()() {
        fflush(stdout);
        if (!read(out_pipe[0], buffer, MAX_LEN))
            buffer[0] = 0;
        return buffer;
    }

    std::string oneline(size_t maxlen) {
        auto val = this->operator()();
        std::replace(val.begin(), val.end(), '\n', ' ');
        if (val.length() > maxlen) {
            val.resize(maxlen - 3);
            val.append("...");
        }
        return val;
    }

    ~CaptureOut() {
        dup2(saved_stdout, STDOUT_FILENO); /* reconnect stdout */
        close(saved_stdout);
        close(out_pipe[0]);
        close(out_pipe[1]);
    }
};
}

#endif
