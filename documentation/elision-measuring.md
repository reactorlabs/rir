# Measuring Elision / Simplification

Specify what to measure by passing (comma-separated) flags on the command-line:

    RIR_MEASURE=
        Envs              how many MkEnv instructions
        Vars              how many variables have loads/stores (by name)
        LazyArgs          how many lazy MkArgs

If any flags are specified, PIR will keep 2 measurements for each closure it compiles. The "initial" measurement is taken once immediately when its compiled (before any optimization passes), and the "new" measurement is constantly updated after each optimization. PIR stores each measurement flag in a separate table, where the rows are the closures, and the columns are **Initial** (amount) and **New**, e.g.

    Vars   , Initial, New
    FooFun ,       2,   1
    Bar    ,       1,   1

There are 2 ways to read these measurements

- **From file:** specify `RIR_MEASURE_FILE` to continuosly export measurements to the given file in CSV form. Useful when measuring `gnur-make-tests`
- **From R:** Call `rir.getMeasure(flag)` to get a data frame of the current flag's measurements. Call `rir.resetMeasure()` to clear old measurements
