#ifndef interpreter_profiler_h
#define interpreter_profiler_h

namespace rir {

class RuntimeProfiler {

  public:
    RuntimeProfiler();
    ~RuntimeProfiler();
    static void initProfiler();
    static RuntimeProfiler& instance();
    void sample(int);
};

} // namespace rir

#endif
