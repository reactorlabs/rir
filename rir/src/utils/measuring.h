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
    template<class F> static ALWAYS_INLINE SEXP
    timeEvent3(const std::string& name, F code) {
        auto timing = startTimingEvent(name);
        auto associated = code();
        PROTECT(associated);
        stopTimingEvent(timing, associated, true);
        UNPROTECT(1);
        return associated;
    }
    template<class F, class F2> static ALWAYS_INLINE SEXP
    timeEvent3(const std::string& name, F code, F2 associatedIsInitialized) {
        auto timing = startTimingEvent(name);
        auto associated = code();
        PROTECT(associated);
        auto isInitialized = associatedIsInitialized(associated);
        stopTimingEvent(timing, associated, isInitialized);
        UNPROTECT(1);
        return associated;
    }
    template<class F> static ALWAYS_INLINE void
    timeEvent(const std::string& name, SEXP associated,
              bool associatedWillBeInitialized, F code) {
        PROTECT(associated);
        auto timing = startTimingEvent(name);
        code();
        stopTimingEvent(timing, associated, associatedWillBeInitialized);
        UNPROTECT(1);
    }
    template<class F> static ALWAYS_INLINE SEXP
    timeEvent2(const std::string& name, SEXP associated,
               bool associatedWillBeInitialized, F code) {
        PROTECT(associated);
        auto timing = startTimingEvent(name);
        auto result = code();
        stopTimingEvent(timing, associated, associatedWillBeInitialized);
        UNPROTECT(1);
        return result;
    }
    template<class F> static ALWAYS_INLINE SEXP
    timeEvent2(const std::string& name, SEXP associated, F code) {
        return timeEvent2(name, associated, true, code);
    }
    template<class F, class F2> static ALWAYS_INLINE SEXP
    timeEventIf3(bool cond, const std::string& name, F code,
                 F2 associatedIsInitialized) {
        if (cond) {
            return timeEvent3(name, code, associatedIsInitialized);
        } else {
            return code();
        }
    }
    template<class F> static ALWAYS_INLINE SEXP
    timeEventIf3(bool cond, const std::string& name, F code) {
        if (cond) {
            return timeEvent3(name, code);
        } else {
            return code();
        }
    }
    template<class F> static ALWAYS_INLINE void
    timeEventIf(bool cond, const std::string& name, SEXP associated,
                bool associatedWillBeInitialized, F code) {
        if (cond) {
            timeEvent(name, associated, associatedWillBeInitialized, code);
        } else {
            code();
        }
    }
    template<class F> static ALWAYS_INLINE SEXP
    timeEventIf2(bool cond, const std::string& name, SEXP associated,
                 bool associatedWillBeInitialized, F code) {
        if (cond) {
            return timeEvent2(name, associated, associatedWillBeInitialized, code);
        } else {
            return code();
        }
    }
    template<class F> static ALWAYS_INLINE SEXP
    timeEventIf2(bool cond, const std::string& name, SEXP associated, F code) {
        return timeEventIf2(cond, name, associated, true, code);
    }
    template<class F> static ALWAYS_INLINE void
    timeEventIf(bool cond, const std::string& name, SEXP associated, F code) {
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
