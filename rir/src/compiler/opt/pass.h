#ifndef PIR_PASS_H
#define PIR_PASS_H

#include "compiler/log/stream_logger.h"
#include "compiler/pir/module.h"

#include <string>

namespace rir {
namespace pir {

class Compiler;

namespace detail {

struct IDCounter {
    static size_t nextId;
};

template <typename Derived>
struct HasIDCounter : IDCounter {
    HasIDCounter() { _id(); }
    static size_t _id() {
        static size_t c = detail::IDCounter::nextId++;
        return c;
    }
};

} // namespace detail

class Pass {
  public:
    explicit Pass(const std::string& name) : name(name) {}

    virtual bool runOnPromises() const { return false; }
    virtual bool isSlow() const { return false; }

    bool apply(Compiler& cmp, ClosureVersion* function, LogStream& log) const;
    virtual bool apply(Compiler& cmp, ClosureVersion*, Code*,
                       LogStream&) const = 0;

    std::string getName() const { return this->name; }
    bool changedAnything() const { return changedAnything_; }
    virtual ~Pass() {}
    virtual bool isPhaseMarker() const { return false; }
    virtual unsigned cost() const { return 1; }
    virtual size_t id() const = 0;

  protected:
    std::string name;
    mutable bool changedAnything_ = false;
};

} // namespace pir
} // namespace rir

#endif
