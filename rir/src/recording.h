#ifndef RECORDING_H
#define RECORDING_H

#include "compiler/pir/closure_version.h"
#include "compiler/pir/pir.h"
#include <R/r.h>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>
namespace rir {

namespace recording {

class Event {
  public:
    friend std::ostream& operator<<(std::ostream& out, const Event& e);

  protected:
    virtual void print(std::ostream&) const = 0;
};

class CompilationEvent : public Event {

    struct pair_hash {
        template <class T1, class T2>
        std::size_t operator()(const std::pair<T1, T2>& pair) const {
            return std::hash<T1>()(pair.first) ^ std::hash<T2>()(pair.second);
        }
    };

    std::unordered_map<std::pair<std::string, unsigned long>, std::string,
                       pair_hash>
        versions;

  public:
    void add_pir_closure_version(const pir::ClosureVersion* version);

  protected:
    void print(std::ostream& out) const;
};

class DeoptEvent : public Event {
  protected:
    void print(std::ostream& out) const {}
};

struct FunRecorder {
    std::string name;
    std::string r_code;
    std::vector<std::unique_ptr<Event>> events;

    friend std::ostream& operator<<(std::ostream& out, const FunRecorder& fr);
};

void record_compile(const SEXP cls, const std::string& name,
                    pir::Module* module);
void record_deopt(const SEXP cls);

} // namespace recording

} // namespace rir

#endif
