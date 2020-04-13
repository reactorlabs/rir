okEnvironment <- as.numeric(Sys.getenv("ENABLE_EVENT_COUNTERS", unset=0)) == 1
okEnvironment <- okEnvironment && as.numeric(Sys.getenv("R_ENABLE_JIT", unset=2)) != 0
okEnvironment <- okEnvironment && (Sys.getenv("PIR_ENABLE", unset="on") == "on")
okEnvironment <- okEnvironment && (as.numeric((Sys.getenv("RIR_WARMUP", unset=3)) == 3)

if (!okEnvironment)
  quit()

rir.flushEventCounters()

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

f(1)
f(2)
f(3)
f(4)
f(5)

for (i in 1:10) {
  f(i)
}

rir.flushEventCounters()

codeEvents <- read.csv("code_events", header = TRUE)
print(codeEvents["# invocations", "f$0"])
print(codeEvents["# invocations", "f$1"])
print(codeEvents["# invocations", "f$2"])