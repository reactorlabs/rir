#ifndef PIR_LOG_H
#define PIR_LOG_H

#include "compiler/pir/pir.h"
#include "debug.h"
#include "loggers.h"

namespace rir {

struct Function;

namespace pir {

class Log {
    static size_t logId;

  public:
    ~Log();

    explicit Log(const DebugOptions& options);
    Log(const Log&) = delete;
    Log& operator=(const Log&) = delete;

    ClosureLog& open(ClosureVersion* cls);
    ClosureLog& get(ClosureVersion* cls) {
        if (!streams.count(cls))
            open(cls);
        return streams.at(cls);
    }

    void title(const std::string& msg);
    void warn(const std::string& msg);

    void flushAll();

    void close(ClosureVersion* cls) {
        auto it = streams.find(cls);
        assert(it != streams.end());
        streams.erase(it);
    }

  private:
    std::unordered_map<ClosureVersion*, ClosureLog> streams;
    DebugOptions options;
};

} // namespace pir
} // namespace rir

#endif
