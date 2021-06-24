#pragma once

#include "R/r_incl.h"

#include "R/Preserve.h"
#include "compiler/log/debug.h"
#include "runtime/Context.h"

#include <condition_variable>
#include <mutex>
#include <thread>
#include <unordered_set>
#include <vector>

namespace rir {
namespace pir {

class CompilerWorker {
  public:
    static CompilerWorker& singleton() {
        static CompilerWorker* w = new CompilerWorker;
        return *w;
    }
    static void shut() { delete &singleton(); }

    typedef std::tuple<std::string, SEXP, Context, pir::DebugOptions> Job;

    CompilerWorker();
    ~CompilerWorker();
    void add(Job);
    void run();

  private:
    bool stop = false;
    void compile(Job job);
    std::vector<Job> todo;
    std::thread thread;
    std::mutex todoMutex;
    std::condition_variable cv;
};

} // namespace pir
} // namespace rir
