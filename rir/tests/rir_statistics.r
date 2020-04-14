okEnvironment <- as.numeric(Sys.getenv("ENABLE_EVENT_COUNTERS", unset=0)) == 1
okEnvironment <- okEnvironment && as.numeric(Sys.getenv("PIR_DEOPT_CHAOS", unset=0)) == 0
okEnvironment <- okEnvironment && as.numeric(Sys.getenv("R_ENABLE_JIT", unset=2)) != 0
okEnvironment <- okEnvironment && (Sys.getenv("PIR_ENABLE", unset="on") == "on")
okEnvironment <- okEnvironment && as.numeric(Sys.getenv("RIR_WARMUP", unset=3)) == 3
usesNativeBackend <- as.numeric(Sys.getenv("PIR_NATIVE_BACKEND", unset=0)) != 0

if (!okEnvironment)
  quit()

getCell <- function(dataFrame, columnName, rowName) {
  cell <- dataFrame[[columnName]][[match(rowName, dataFrame[[1]])]]
  stopifnot(!is.null(cell))
  cell
}

getEventCell <- function(dataFrame, rowName) {
  getCell(dataFrame, 2, rowName)
}

rir.resetEventCounters()

g <- function(x) {
  3 * x
}

f <- function(x) {
  y <- 0
  for (i in 1:10) {
    y <- y + g(x)
  }
  5 + y
}

# we call the unoptimized version twice to warmup...
f(1)
f(2)
# and then the optimized version...
f(3)
f(4)
f(5)
f(6)
f(7)

# now we have weaker assumptions.
# First we will optimize, but then we actually deoptimize once because of a type speculation failure (we expect real but get int; i = 1).
# Then we immediately optimize a version with fixed type feedback (i = 2..10)
for (i in 1:10) {
  f(i)
}

deopt <- function(start, end) {
  for (i in start:end) {
    summary(i)
  }
}

lhs <- 4
rhs <- 10
# again, we call unoptimized to warmup...
deopt(lhs, rhs)
deopt(lhs, rhs)
# and then call the optimized version
deopt(lhs, rhs)
rhs <- 7
deopt(lhs, rhs)
deopt(lhs, rhs)
deopt(lhs, rhs)
rhs <- 1
# we deopt here and compile a new version because we reversed the loop counter,
# which causes a different branch, and we speculated on the old branch
deopt(lhs, rhs)
# optimize again
deopt(lhs, rhs)
deopt(lhs, rhs)
lhs <- factor(lhs)
rhs <- factor(rhs)
# we deopt again, because lhs and rhs are factors so we skip the fastcase
deopt(lhs, rhs)
# optimize again
deopt(lhs, rhs)
deopt(lhs, rhs)

profiled <- function(x) {
  vector <- integer(x)
  for (idx in 1L:x) {
    vector[[idx]] <- idx
  }
  5
}

startTime <- as.numeric(Sys.time())
profiled(1000000)
profiled(1000000)
endTime <- as.numeric(Sys.time())
profiledTimeInR <- endTime - startTime
profiled(1)

rir.flushEventCounters()

events <- as.data.frame(read.csv("events.csv", header = TRUE))
codeEvents <- as.data.frame(read.csv("code_events.csv", header = TRUE))
numClosuresPerTable <- as.data.frame(read.csv("num_closures_per_table.csv", header = TRUE))

stopifnot(getCell(codeEvents, "X..invocations", "f$0") == 2)
stopifnot(getCell(codeEvents, "X..invocations", "f$1") == 1)
stopifnot(getCell(codeEvents, "X..invocations", "f$1~1") == 9)
# when we have different assumptions, we compile a new version but keep the old one
stopifnot(getCell(codeEvents, "X..invocations", "f$2") == 5)
# g's invocation counts are weird. In the baseline of f, we call the baseline of g twice,
# then optimize and call 18 more times (since we call the baseline of f 2 times, we call g 20 times total).
# Then we optimize f and inline g, so it's no longer called.
# Except we end up calling the baseline of f one more time (because of a deopt) after calling an inlined g once,
# and this causes us to call g one more time (in the for loop in f, when i = 2), deoptimize,
# and then compile another version of g, which we call 8 times (i = 3..10).
stopifnot(getCell(codeEvents, "X..invocations", "g$0") == 2)
stopifnot(getCell(codeEvents, "X..invocations", "g$1") == 19)
stopifnot(getCell(codeEvents, "X..invocations", "g$1~1") == 8)

# We can't really track callsites to f and deopt because they are called from the REPL. We also can't accurately track in the native backend
if (!usesNativeBackend) {
  # There are 2 callsites to g because of loop peeling. Except g$1~1 only has one callsite because when it's compiled we have already entered the loop
  stopifnot(getCell(codeEvents, "X..distinct.callsites", "g$0") == 2)
  stopifnot(getCell(codeEvents, "X..distinct.callsites", "g$1") == 2)
  stopifnot(getCell(codeEvents, "X..distinct.callsites", "g$1~1") == 1)
}

stopifnot(getCell(codeEvents, "X..invocations", "deopt$0") == 2)
stopifnot(getCell(codeEvents, "X..invocations", "deopt$1") == 5)
# when we have deoptimize, we remove the old version (hence the $1 because there are still only 2 versions total)
stopifnot(getCell(codeEvents, "X..invocations", "deopt$1~1") == 3)
stopifnot(getCell(codeEvents, "X..invocations", "deopt$1~2") == 2)

profiledTimeInCsv <- getCell(codeEvents, "total.execution.time..µs.", "profiled$0") / 1000000
profiledTimeDiff <- abs(profiledTimeInR - profiledTimeInCsv)
# should not have more than 1ms difference
stopifnot(profiledTimeDiff < 0.001)

stopifnot(getCell(numClosuresPerTable, "final.size", "f") == 3)
stopifnot(getCell(numClosuresPerTable, "X..deopts", "f") == 1)
stopifnot(getCell(numClosuresPerTable, "final.size", "g") == 2)
stopifnot(getCell(numClosuresPerTable, "X..deopts", "g") == 1)
stopifnot(getCell(numClosuresPerTable, "final.size", "deopt") == 2)
stopifnot(getCell(numClosuresPerTable, "X..deopts", "deopt") == 2)

stopifnot(getEventCell(events, "deopt") == 4)
# summary is unoptimizable (it's a generic with ... arguments), all other functions are optimizable
stopifnot(getEventCell(events, "unoptimizable") == 1)

# So we keep the flushed code_events.csv, helpful for debugging
rir.resetEventCounters()