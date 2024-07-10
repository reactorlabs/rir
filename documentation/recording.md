# Record Events

To have recording enabled, Cmake must be configured with `-DRECORDING=1`, for example

    cmake -GNinja -DCMAKE_BUILD_TYPE=debug -DRECORDING=1 ..

To disable it afterwards, use the `-DRECORDING=0` flag.

## Events

Curently, there are five different events:

*CompilationStart*  
Start of a compilation session, with the reason (ORS/condition and heuristic)

*Compilation*  
A compilation of function from RIR to PIR was invoked.

*CompilationEnd*  
Succesful end of a compilation session with the time it took to compile (not exposed in CSV).

*CompilationAborted*  
Unsuccesful end of a compilation session.

*Deopt*  
A deopt from native code has been raised.

*Invocation*  
An invocation of RIR or PIR code.

*UnregisterInvocation*  
An invocation of a function has been rolled back.

*SpeculativeContext*  
A type feedback has been updated in a function on a given slot.

*ContextualTypeFeedback*  
A new contextual type feedback slot has been created.

### Event filtering

By default, only *compilations* and *deopts* are recorded.  
To change which events are recorded, see the `recordings.setFilter` or `RIR_RECORD_FILTER` options below.

The options then correspond as follows:
- compile - compilation events
- deopt - deopt events
- invoke - invocation and unregister invocation events
- type feedback - the speculative context events
- contextual type feedback - the contextual type feedback creation events

## Enabling recording

Both environment flags and R API can be used together, for example starting the recording with `RIR_RECORD`, then pausing it with
`recordings.stop()`

### R API
There is an R-level API for recording:

`recordings.start()` - resumes the recording, or starts it if it was not paused  
`recordings.stop()` - pauses the recording  
`recordings.get()` - returns the object with recorded functions and events  

`recordings.save(filename)` - saves the recording as RDS to the given file  
`recordings.load(filename)` - loads the recording from the given file and returns the object representing it  

`recordings.reset()` - clear the logged functions, closures and events  
`recordings.enabled()` - boolean representing if we are recording right now  

`recordings.setFilter(compile, deoptimize, type_feedback, invocation, context_tf)` - set the filtering of individual events  

### Recording flags

To record the whole run of R, run the script as follows:

    RIR_RECORD=output.rds ./bin/R -f test.R

where `output.rds` is the destination file where the recording will be saved.

With the `RIR_RECORD_FILTER` environment variable, you can also specify a recording filter,
with comma separated values of:
- Compile
- Deopt
- TypeFeedback
- Invoke
- ContextualTypeFeedback

As an example:

    RIR_RECORD=output.rds RIR_RECORD_FILTER=Compile,TypeFeedback ./bin/R -f test.R

turns on the recording of compilations and type feedback updates.

## Recording object structure
The object returned by `recordings.get` or `recordings.load` is a representation of the whole log.  
It has two attributes:

- `$functions` - a list of all recorded functions, with fields:
    - `primIdx` - a -1 if not a primitive, or an index into `R_FunTab`
    - `name` - the name of the function (if any)
    - `env` - the name of the enviroment
    - `closure` - the serialized closure
- `$events` - a list of recorded events.
    - Individual types of events are disambiguated by their `class` attribute.
    - Each of them also has a `$funIdx` attribute, which is an index to the `$functions`,
    specifying which functions is the event connected with.
    - Some of them also have a `$version` or `$context` attribute, meaning they are connected to a specific version
    of the function.


## Pretty printing
For formatting into CSV, the file `replayer.r` contains one function, `recordings.csv(r, out)`.  
The `r` paramater is either a recording object or a path to the RDS file representing the recording.
It then outputs a CSV representation of the events (comma separated, with quoted fields when needed).
When no `out` parameter is specified, it prints to the standard output. Otherwise, it prints into the specified file.  

It can be used from R as follows:

    require("replayer.r")
    recordings.csv("output.rds", out="log.csv")


### CSV Columns
The individual meanings of printed columns:
- idx - the index of the event
- type - type (name) of the event
- fun - the function name it is connected with (or address for simple expressions)
- env - name of the environment of the function/expression
- ctx - the context of function that was compiled/deopted/invoked
- speculative_ctx - (on the the multiple type feedbacks branch) the context, by which the type feedback was dispatched
- speculative - the type feedback when compiling, or the new state and slot index of updated type feedback
- call_ctx - the context the function was called with, but not the one actually used
- reason - deopt reason / compilation reason and heuristic / invocation source
- bitcode_len - lenght of compiled LLVM bitcode
- pir_len - length of compiled and optimized PIR code
- changed - is the type feedback actually changed
- is_promise - is the type feedback inside of a promise
- is_native - is the invoked function a native one
- callee_address - address of called function

