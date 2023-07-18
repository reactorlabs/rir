#ifndef MEASURING_H
#define MEASURING_H

#include "R/r_incl.h"
#include <string>

namespace rir {

class Measuring {
    struct TimingEvent;

    static TimingEvent* startTimingEvent(const std::string& name, SEXP associated);
    static void stopTimingEvent(TimingEvent* timing, bool associatedIsInitialized);
  public:
    static inline void timeEvent(const std::string& name, SEXP associated,
                                 bool associatedWillBeInitialized,
                                 const std::function<void()>& code) {
        auto timing = startTimingEvent(name, associated);
        code();
        stopTimingEvent(timing, associatedWillBeInitialized);
    }
    template<typename T> static inline T
    timeEvent(const std::string& name, SEXP associated,
              bool associatedWillBeInitialized,
              const std::function<T()>& code) {
        auto timing = startTimingEvent(name, associated);
        auto result = code();
        stopTimingEvent(timing, associatedWillBeInitialized);
        return result;
    }
    static inline void timeEvent(const std::string& name, SEXP associated,
                                 const std::function<void()>& code) {
        timeEvent(name, associated, true, code);
    }
    template<typename T> static inline T
    timeEvent(const std::string& name, SEXP associated,
              const std::function<T()>& code) {
        return timeEvent<T>(name, associated, true, code);
    }
    static inline void timeEventIf(bool cond, const std::string& name,
                                   SEXP associated,
                                   bool associatedWillBeInitialized,
                                   const std::function<void()>& code) {
        if (cond) {
            timeEvent(name, associated, associatedWillBeInitialized, code);
        } else {
            code();
        }
    }
    template<typename T> static inline T
    timeEventIf(bool cond, const std::string& name, SEXP associated,
                bool associatedWillBeInitialized,
                const std::function<T()>& code) {
        if (cond) {
            return timeEvent(name, associated, associatedWillBeInitialized, code);
        } else {
            return code();
        }
    }
    static inline void timeEventIf(bool cond, const std::string& name,
                                   SEXP associated,
                                   const std::function<void()>& code) {
        timeEventIf(cond, name, associated, true, code);
    }
    template<typename T> static inline T
    timeEventIf(bool cond, const std::string& name, SEXP associated,
                const std::function<T()>& code) {
        return timeEventIf<T>(cond, name, associated, true, code);
    }

    static void startTimer(const std::string& name);
    static void countTimer(const std::string& name);
    static void addTime(const std::string& name, double time);
    static inline void startTimerIf(bool cond, const std::string& name) {
        if (cond) {
            startTimer(name);
        }
    }
    static inline void countTimerIf(bool cond, const std::string& name) {
        if (cond) {
            countTimer(name);
        }
    }

    static void setEventThreshold(size_t n);
    static void countEvent(const std::string& name, size_t n = 1);

    static void reset(bool outputOld = false);
};

} // namespace rir

#endif
