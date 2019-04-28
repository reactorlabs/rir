# Measuring Elision / Simplification

Specify what to measure by passing (comma-separated) flags on the command-line:

    RIR_MEASURE=
        Envs        how often a closure creates an env / how often it's called
        Vars        how many vars have loads (by name) before / after passes
        LazyArgs    how many lazy MkArgs before / after optimization passes

PIR stores each measurement flag in a separate table, where each row is a closures, e.g.

    Closure, Optimized, Initial
    FooFun ,         2,       1
    Bar    ,         1,       1

There are 2 ways to read these measurements

- **From file:** specify `RIR_MEASURE_FILE` to continuosly export measurements to a file in CSV form. The file will be named `<RIR_MEASURE_FILE>_<flag><pid>.csv`. Useful when measuring `gnur-make-tests`
- **From R:** Call `rir.getMeasure(flag)` to get a data frame of the current flag's measurements. Call `rir.resetMeasure()` to clear old measurements
