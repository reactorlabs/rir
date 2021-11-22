#ifndef MEASURING_H
#define MEASURING_H

#include <string>

namespace rir {

class Measuring {
  public:
    static void startTimer(const std::string& name);
    static void countTimer(const std::string& name);
    static void addTime(const std::string& name, double time);
    static void setEventThreshold(size_t n);
    static void countEvent(const std::string& name, size_t n = 1);
    static void reset(bool outputOld = false);
};

} // namespace rir

#endif
