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

static RuntimeProfiler instance;

RuntimeProfiler::RuntimeProfiler() {}

RuntimeProfiler::~RuntimeProfiler() {}

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
    auto md = code->pirRegisterMap();
    if (!md)
        return;
    md->forEachSlot([stack](size_t i, PirRegisterMap::MDEntry& mdEntry) {
        auto slot = *(stack + i);
        assert(slot.tag == 0);
        if (auto sxpval = slot.u.sxpval) {
            mdEntry.feedback.record(sxpval);
            auto samples = ++(mdEntry.sampleCount);
            if (samples == 10) {
                mdEntry.readyForReopt = true;
            }
            if (samples > 100) {
                mdEntry.readyForReopt = false;
                mdEntry.sampleCount = 0;
                mdEntry.feedback.reset();
            }
        }
    });
}

#ifndef __APPLE__
static void handler(int signal) { instance.sample(signal); }

void RuntimeProfiler::initProfiler() {
    bool ENABLE_PROFILER = true; // getenv("PIR_ENABLE_PROFILER") ? true :
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
}
#else
void RuntimeProfiler::initProfiler() {}
#endif

} // namespace rir
