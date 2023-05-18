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
    static std::unique_ptr<Event> read_any(char* file);
    virtual void write(FILE* file) const = 0;
    virtual void read(char* file) = 0;
    virtual void replay(SEXP cls, char* cls_name) const = 0;

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
    void write(FILE* file) const override;
    void read(char* file) override;
    void replay(SEXP cls, char* cls_name) const override;

    Context assumptions;

  protected:
    void print(std::ostream& out) const;
};

class DeoptEvent : public Event {
  public:
    void write(FILE* file) const override;
    void read(char* file) override;
    void replay(SEXP cls, char* cls_name) const override;

  protected:
    void print(std::ostream& out) const {}
};

struct FunRecorder {
    std::string name;
    std::string r_code;
    std::vector<std::unique_ptr<Event>> events;

    friend std::ostream& operator<<(std::ostream& out, const FunRecorder& fr);
};

void record_compile(SEXP const cls, const std::string& name,
                    pir::Module* module, const Context& assumptions);
void record_deopt(const SEXP cls);

size_t saveTo(FILE* file);
size_t replayFrom(FILE* file);

} // namespace recording

} // namespace rir

#endif
