#include "api.h"

#include "interp.h"
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

#include <iomanip>
#include <unordered_map>

#include "profiler.h"

#include <time.h>

namespace rir {

RuntimeProfiler::RuntimeProfiler() {
}

RuntimeProfiler::~RuntimeProfiler() {
}

void RuntimeProfiler::sample(int signal) {
    auto& ctx = R_GlobalContext;
    auto& stack = ctx->nodestack;
    if (R_BCNodeStackTop == R_BCNodeStackBase)
        return;
    if ((stack)->tag != 0)
        return;
    auto tos = (stack)->u.sxpval;
    if (!tos)
        return;
    auto code = Code::check(tos);
    if (!code)
        return;
    auto md = code->getValueProfilerMetadata();
    if (!md)
        return;
    for (size_t i = 0; i < code->getValueProfilerMetadataSize(); ++i) {
        if (md[i].active) {
            auto slot = *(stack + i);
            assert(slot.tag == 0);
            if (slot.u.sxpval) {
                md[i].feedback.record(slot.u.sxpval);
                auto samples = ++(md[i].sample_count);
                if (samples == 10) {
                    md[i].ready_for_reopt = true;
                }
                if (samples > 100) {
                    md[i].ready_for_reopt = false;
                    md[i].sample_count = 0;
                    std::memset(&(md[i].feedback), 0,
                                sizeof(struct ObservedValues));
                }
            }
        }
    }
}

static void handler(int signal) { RuntimeProfiler::instance().sample(signal); }

void RuntimeProfiler::initProfiler() {
    #ifndef __APPLE__
    bool ENABLE_PROFILER = false; // getenv("PIR_ENABLE_PROFILER") ? true :
                                 // false;
    if (!ENABLE_PROFILER) {
        return;
    }
    struct sigaction recordAction;
    struct sigevent sev;
    struct itimerspec itime;
    timer_t timer_id;

    recordAction.sa_handler = handler;
    sigfillset(&recordAction.sa_mask);
    recordAction.sa_flags = SA_RESTART;
    sigaction(SIGUSR1, &recordAction, NULL);

    // create timer

    sev.sigev_notify = SIGEV_SIGNAL;
    sev.sigev_signo = SIGUSR1;
    sev.sigev_value.sival_ptr = &timer_id;

    timer_create(CLOCK_MONOTONIC, &sev, &timer_id);

    itime.it_value.tv_sec = 0;
    /* 500 million nsecs = .5 secs */
    itime.it_value.tv_nsec = 10000000;
    itime.it_interval.tv_sec = 0;
    /* 500 million nsecs = .5 secs */
    itime.it_interval.tv_nsec = 10000000;
    timer_settime(timer_id, 0, &itime, NULL);
    #endif
}

RuntimeProfiler& RuntimeProfiler::instance() {
    static std::unique_ptr<RuntimeProfiler> singleton(new RuntimeProfiler);
    return *singleton;
}

} // namespace rir
