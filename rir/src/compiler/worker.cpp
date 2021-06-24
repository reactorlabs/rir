#include "worker.h"

#include "R/Protect.h"
#include "compiler/backend.h"
#include "compiler/compiler.h"
#include "compiler/log/debug.h"
#include "runtime/DispatchTable.h"

namespace rir {
namespace pir {

static void spawn(CompilerWorker* w) { w->run(); }

CompilerWorker::CompilerWorker() : thread(spawn, this) {}

CompilerWorker::~CompilerWorker() {
    {
        std::unique_lock<std::mutex> lck(todoMutex);
        stop = true;
        todo.clear();
        cv.notify_one();
    }
    thread.join();
}

void CompilerWorker::run() {
    while (!stop) {
        Job job;
        {
            std::unique_lock<std::mutex> lck(todoMutex);
            cv.wait(lck, [&]() { return stop || !todo.empty(); });

            if (todo.empty())
                continue;

            std::cout << "* CompilerWorker: got job\n";
            job = todo.back();
            todo.pop_back();
        }
        compile(job);
    }
    std::cout << "* CompilerWorker: exit\n";
}

void CompilerWorker::compile(Job job) {
    auto what = std::get<SEXP>(job);
    auto debug = std::get<pir::DebugOptions>(job);
    auto name = std::get<std::string>(job);
    auto assumptions = std::get<Context>(job);

    bool dryRun = debug.includes(pir::DebugFlag::DryRun);
    // compile to pir
    pir::Module* m = new pir::Module;
    pir::StreamLogger logger(debug);
    logger.title("Compiling " + name);
    pir::Compiler cmp(m, logger);
    pir::Backend backend(logger, name);
    cmp.compileClosure(what, name, assumptions, true,
                       [&](pir::ClosureVersion* c) {
                           logger.flush();
                           cmp.optimizeModule();

                           /*auto fun =*/backend.getOrCompile(c);

                           // Install
                           if (dryRun)
                               return;

                           // TODO: make DispatchTable::insert thread safe
                           // DispatchTable::unpack(BODY(what))->insert(fun);
                           std::cout << "* CompilerWorker: done compiling "
                                     << name << "\n";
                       },
                       [&]() {
                           if (debug.includes(pir::DebugFlag::ShowWarnings))
                               std::cerr << "Compilation failed\n";
                       },
                       {});

    delete m;
}

void CompilerWorker::add(Job j) {
    auto cls = std::get<SEXP>(j);

    // Schedule a new job
    {
        bool alreadyScheduled = false;
        std::unique_lock<std::mutex> lck(todoMutex);

        // Check if job is already scheduled
        for (auto j2 : todo) {
            if (std::get<SEXP>(j2) == cls) {
                alreadyScheduled = true;
                break;
            }
        }
        if (!alreadyScheduled) {
            auto dt = DispatchTable::unpack(BODY(cls));
            dt->compiling++;
            R_PreserveObject(std::get<SEXP>(j));
            todo.insert(todo.begin(), j);
            cv.notify_one();
        }
    }
}

} // namespace pir
} // namespace rir
