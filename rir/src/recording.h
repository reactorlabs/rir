#ifndef RECORDING_H
#define RECORDING_H

#include <R/r.h>
#include <string>
#include <vector>
namespace rir {

namespace recording {

class Event {};

class CompilationEvent : public Event {};
class DeoptEvent : public Event {};

struct FunRecorder {
    std::string name;
    std::string r_code;
    std::vector<Event> events;
};

void record_compile(SEXP cls, const std::string& name);
void record_deopt(SEXP cls);

} // namespace recording

} // namespace rir

#endif
