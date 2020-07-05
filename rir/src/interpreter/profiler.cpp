#include "api.h"

#include "interp.h"
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

#include <iomanip>
#include <unordered_map>

#include "compiler/pir/type.h"
#include "profiler.h"

#ifndef __APPLE__
#include <asm/unistd.h>
#include <fcntl.h>
#include <linux/perf_event.h>
#include <sys/ioctl.h>
#include <time.h>
#endif

long perf_event_open(struct perf_event_attr* event_attr, pid_t pid, int cpu,
                     int group_fd, unsigned long flags) {
    return syscall(__NR_perf_event_open, event_attr, pid, cpu, group_fd, flags);
}

namespace rir {

static RuntimeProfiler instance;

static volatile size_t samples = 0;
static volatile size_t hits = 0;
static volatile size_t compilations = 0;

RuntimeProfiler::RuntimeProfiler() {}

RuntimeProfiler::~RuntimeProfiler() {}

static bool needReopt = false;
static size_t goodValues = 0;
static size_t slotCount = 0;
static R_bcstack_t* stack;

void RuntimeProfiler::sample(int signal) {
    samples++;
    auto ctx = (RCNTXT*)R_GlobalContext;
    stack = ctx->nodestack;
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
    auto md = code->pirTypeFeedback();
    if (!md)
        return;

    hits++;
    needReopt = false;
    goodValues = 0;
    slotCount = 0;

    md->forEachSlot([](size_t i, PirTypeFeedback::MDEntry& mdEntry) {
        auto slot = *(stack + i);
        assert(slot.tag == 0);
        if (auto sxpval = slot.u.sxpval) {
            mdEntry.feedback.record(sxpval);
            auto samples = ++(mdEntry.sampleCount);
            slotCount++;
            if (samples == 10) {
                mdEntry.readyForReopt = true;
                // check if this feedback justifies a reopt
                pir::PirType after;
                after.merge(mdEntry.feedback);
                if (!mdEntry.previousType.isA(after)) {
                    // mark slot as good for reopt
                    mdEntry.needReopt = true;
                }
            }
            if (samples >= 10) {
                goodValues++;
                if (mdEntry.needReopt) {
                    needReopt = true;
                }
            }
            if (samples > 100) {
                mdEntry.readyForReopt = false;
                mdEntry.sampleCount = 0;
                mdEntry.feedback.reset();
            }
        }
    });

    // only trigger reopt if at least 50% of all slots have enough samples and
    // at least one slot justifies re-opt.
    if (goodValues >= (slotCount / 2) && needReopt) {
        // set global re-opt flag
        code->flags.set(Code::Reoptimise);
        compilations++;
    }
}

#ifndef __APPLE__
static void handler(int signal) { instance.sample(signal); }

static void dump() {
    std::cout << "\nsamples: " << samples << ", hits: " << hits << "\n"
              << "triggered " << compilations << " recompilations\n";
}

void RuntimeProfiler::initProfiler() {
    bool ENABLE_PROFILER = getenv("PIR_ENABLE_PROFILER") ? true : false;
    if (!ENABLE_PROFILER) {
        return;
    }

    std::atexit(dump);

    // Configure signal handler
    struct sigaction sa;
    memset(&sa, 0, sizeof(struct sigaction));
    sa.sa_handler = handler;
    sa.sa_flags = 0; // SA_SIGINFO;

    // Setup signal handler
    if (sigaction(SIGUSR1, &sa, NULL) < 0) {
        fprintf(stderr, "Error setting up signal handler\n");
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    // Configure PMU
    struct perf_event_attr pe;
    memset(&pe, 0, sizeof(struct perf_event_attr));
    pe.type = PERF_TYPE_HARDWARE;
    pe.size = sizeof(struct perf_event_attr);
    pe.config =
        PERF_COUNT_HW_INSTRUCTIONS; // Count retired hardware instructions
    pe.disabled = 1;                // Event is initially disabled
    pe.sample_type = PERF_SAMPLE_IP;
    pe.sample_period = 1000000;
    pe.exclude_kernel = 1; // excluding events that happen in the kernel-space
    pe.exclude_hv = 1;     // excluding events that happen in the hypervisor
    pe.precise_ip = 3;

    int fd = perf_event_open(&pe, 0, -1, -1, 0);
    if (fd == -1) {
        fprintf(stderr, "Error opening leader %llx\n", pe.config);
        perror("perf_event_open");
        exit(EXIT_FAILURE);
    }
    // pmu_fd = fd;

    // Setup event handler for overflow signals
    fcntl(fd, F_SETFL, O_NONBLOCK | FASYNC);
    fcntl(fd, F_SETSIG, SIGUSR1);
    fcntl(fd, F_SETOWN, getpid());

    ioctl(fd, PERF_EVENT_IOC_RESET, 0);    // Reset event counter to 0
    ioctl(fd, PERF_EVENT_IOC_REFRESH, -1); // Allow first signal
}

#else
void RuntimeProfiler::initProfiler() {}
#endif

} // namespace rir
