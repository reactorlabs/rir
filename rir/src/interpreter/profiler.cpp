#include "api.h"

#include "interp.h"
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

#include <iomanip>
#include <unordered_map>

#include "profiler.h"

#include <time.h>

namespace rir {

  RuntimeProfiler::RuntimeProfiler() {
    printf("### HELLO\n");
    this->counter = 0;
    this->counter2 = 0;
  }

  RuntimeProfiler::~RuntimeProfiler() {
    printf("### BYE\nSignals: %d\nRecorded Feedback: %d\n", this->counter, this->counter2);
  }

  void RuntimeProfiler::sample(int signal) {
    this->counter++;

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
        auto& metadata = md[i];
	if (metadata.active) {
            auto slot = *(stack + i);
            assert(slot.tag == 0);
            if (slot.u.sxpval) {
              metadata.feedback.record(slot.u.sxpval);
	      auto samples = ++(metadata.sample_count);
              if (samples == 10) {
	        metadata.ready_for_reopt = true;
	      }
              if (samples > 100) {
		 metadata.ready_for_reopt = false;
                 metadata.sample_count = 0;
                 std::memset(&(metadata.feedback), 0, sizeof(struct ObservedValues));
	      }
              this->counter2++;
            }
        }
    }
  }

  static void handler(int signal) {
    RuntimeProfiler::instance().sample(signal);
  }


  void RuntimeProfiler::initProfiler() {
    printf("profiler?\n");
    bool ENABLE_PROFILER = true;  //getenv("PIR_ENABLE_PROFILER") ? true : false;
    if (!ENABLE_PROFILER) {
      printf("no!\n");
      return;
    }
    struct sigaction recordAction;
    struct sigevent sev;
    struct itimerspec itime;
    timer_t timer_id;

    printf("registering profiler signal handler\n");
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

  RuntimeProfiler& RuntimeProfiler::instance() {
    static std::unique_ptr<RuntimeProfiler> singleton(new RuntimeProfiler);
    return *singleton;
  }


}
