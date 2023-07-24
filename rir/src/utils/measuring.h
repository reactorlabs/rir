#ifndef MEASURING_H
#define MEASURING_H

#include "R/r_incl.h"
#include <string>

namespace rir {

class Measuring {
    struct TimingEvent;

    static TimingEvent* startTimingEvent(const std::string& name);
    static void stopTimingEvent(TimingEvent* timing, SEXP associated,
                                bool associatedIsInitialized);
  public:
    static inline SEXP timeEvent(const std::string& name,
                                 const std::function<SEXP()>& code,
                                 const std::function<bool(SEXP associated)>&
                                     associatedIsInitialized) {
        auto timing = startTimingEvent(name);
        auto associated = code();
        PROTECT(associated);
        auto isInitialized = associatedIsInitialized(associated);
        stopTimingEvent(timing, associated, isInitialized);
        UNPROTECT(1);
        return associated;
    }
    static inline void timeEvent(const std::string& name, SEXP associated,
                                 bool associatedWillBeInitialized,
                                 const std::function<void()>& code) {
        PROTECT(associated);
        auto timing = startTimingEvent(name);
        code();
        stopTimingEvent(timing, associated, associatedWillBeInitialized);
        UNPROTECT(1);
    }
    template<typename T> static inline T
    timeEvent(const std::string& name, SEXP associated,
              bool associatedWillBeInitialized,
              const std::function<T()>& code) {
        PROTECT(associated);
        auto timing = startTimingEvent(name);
        auto result = code();
        stopTimingEvent(timing, associated, associatedWillBeInitialized);
        UNPROTECT(1);
        return result;
    }
    template<typename T> static inline T
    timeEvent(const std::string& name, SEXP associated,
              const std::function<T()>& code) {
        return timeEvent(name, associated, true, code);
    }
    static inline SEXP timeEventIf(bool cond, const std::string& name,
                                   const std::function<SEXP()>& code,
                                   const std::function<bool(SEXP associated)>&
                                       associatedIsInitialized = [](SEXP _s){ return true; }) {
        if (cond) {
            return timeEvent(name, code, associatedIsInitialized);
        } else {
            return code();
        }
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
    template<typename T> static inline T
    timeEventIf(bool cond, const std::string& name, SEXP associated,
                const std::function<T()>& code) {
        return timeEventIf(cond, name, associated, true, code);
    }
    static inline void timeEventIf(bool cond, const std::string& name,
                                   SEXP associated,
                                   const std::function<void()>& code) {
        timeEventIf(cond, name, associated, true, code);
    }


    static void startTimer(const std::string& name, bool canNest = false);
    static void countTimer(const std::string& name, bool canNest = false);
    static void addTime(const std::string& name, double time);
    static inline void startTimerIf(bool cond, const std::string& name,
                                    bool canNest = false) {
        if (cond) {
            startTimer(name, canNest);
        }
    }
    static inline void countTimerIf(bool cond, const std::string& name,
                                    bool canNest = false) {
        if (cond) {
            countTimer(name, canNest);
        }
    }

    static void setEventThreshold(size_t n);
    static void countEvent(const std::string& name, size_t n = 1);

    static void reset(bool outputOld = false);
};

} // namespace rir

#endif
