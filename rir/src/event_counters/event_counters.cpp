#include "event_counters.h"

namespace rir {

bool EventCounters::isEnabled = (getenv("ENABLE_EVENT_COUNTERS") &&
                                 *getenv("ENABLE_EVENT_COUNTERS") == '1');
}