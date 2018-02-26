#ifndef CAPTURE_OUT_H
#define CAPTURE_OUT_H

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>

namespace rir {

class CaptureOut {
    const static unsigned MAX_LEN = 1024 * 8;
    char buffer[MAX_LEN + 1] = {0};
    int out_pipe[2];
    int saved_stdout;

  public:
    CaptureOut() {
        fflush(stdout);
        saved_stdout = dup(STDOUT_FILENO);
        if (pipe(out_pipe) != 0)
            return;
        dup2(out_pipe[1], STDOUT_FILENO);
        close(out_pipe[1]);
    }

    std::string operator()() {
        fflush(stdout);
        read(out_pipe[0], buffer, MAX_LEN);
        return buffer;
    }

    ~CaptureOut() {
        dup2(saved_stdout, STDOUT_FILENO); /* reconnect stdout for testing */
    }
};
}

#endif
