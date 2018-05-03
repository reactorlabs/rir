#ifndef CAPTURE_OUT_H
#define CAPTURE_OUT_H

#include <cstdio>
#include <cstdlib>
#include <cstring>
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
    char buffer[MAX_LEN + 1] = {0};
    int out_pipe[2];
    int saved_stdout;

  public:
    CaptureOut() {
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
        read(out_pipe[0], buffer, MAX_LEN);
        return buffer;
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
