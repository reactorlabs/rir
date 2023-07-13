#ifndef MEASURING_H
#define MEASURING_H

#include "R/r_incl.h"
#include <string>

namespace rir {

class Measuring {
    struct TimingEvent;

    static TimingEvent* startTimingEvent(const std::string& name, SEXP associated);
    static void stopTimingEvent(TimingEvent* timing);
  public:
    static inline void timeEvent(const std::string& name, SEXP associated,
                                 const std::function<void()>& code) {
        auto timing = startTimingEvent(name, associated);
        code();
        stopTimingEvent(timing);
    }
    template<typename T> static inline T
    timeEvent(const std::string& name, SEXP associated,
              const std::function<T()>& code) {
        auto timing = startTimingEvent(name, associated);
        auto result = code();
        stopTimingEvent(timing);
        return result;
    }
    static inline void timeEventIf(bool cond, const std::string& name,
                                   SEXP associated,
                                   const std::function<void()>& code) {
        if (cond) {
            timeEvent(name, associated, code);
        } else {
            code();
        }
    }
    template<typename T> static inline T
    timeEventIf(bool cond, const std::string& name, SEXP associated,
                const std::function<T()>& code) {
        if (cond) {
            return timeEvent(name, associated, code);
        } else {
            return code();
        }
    }

    static void startTimer(const std::string& name);
    static void countTimer(const std::string& name);
    static void addTime(const std::string& name, double time);

    static void setEventThreshold(size_t n);
    static void countEvent(const std::string& name, size_t n = 1);

    static void reset(bool outputOld = false);
};

} // namespace rir

#endif
