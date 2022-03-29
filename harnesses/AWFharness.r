innerBenchmarkLoop.default <- function(class, iterations) {
  for (i in 1:iterations) {
    if (!verifyResult(execute(), iterations)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

innerBenchmarkLoop <- function(x, ...) {
  UseMethod("innerBenchmarkLoop", x)
}

doRuns <- function(name, iterations, innerIterations) {
  total <- 0
  class(name) = tolower(name)
  path <- paste("/opt/rir/harnesses/bitcodes/", gsub("/","_",name), "/", sep ="")
  Sys.setenv(PIR_DESERIALIZE_PREFIX = path)

  startTime <- Sys.time()
  f.loadBitcodes()
  endTime <- Sys.time()

  for (i in 1:iterations) {
    startTime =  Sys.time()
    f.startSerializer()
    if (!innerBenchmarkLoop(name, innerIterations)) {
      stop ("Benchmark failed with incorrect result")
    }
    f.stopSerializer()
    endTime <- Sys.time()
    runTime = (as.numeric(endTime) - as.numeric(startTime)) * 1000000

    cat(paste(paste(paste(name, ": iterations=1 runtime:", sep = ""),
            round(runTime)), "us\n", sep = ""))
    total = total + runTime
  }
  return (total)
}

run <- function(args) {
  if (length(args) == 0)
    stop(printUsage())

  name = args[1]

  numIterations = 1
  innerIterations = 1

  if (length(args) > 1) {
    numIterations = strtoi(args[2])
    if (length(args) > 2) {
      innerIterations = strtoi(args[3]);
    }
  }

  source(paste(paste("./", tolower(name), sep = ""), ".r", sep = ""))

  total = as.numeric(doRuns(name, numIterations, innerIterations));
  cat(paste(paste(name, ": iterations=", sep=""), numIterations, sep=""))
  cat(paste("; average:", round(total / numIterations)))
  cat(paste(paste(" us; total:", round(total)), "us\n\n", sep=""))
  #cat("\n")
  #cat(paste(paste("Total Runtime:", round(total)), "us\n\n", sep=""))
}

printUsage <- function() {
  cat("harness.r [benchmark] [num-iterations [inner-iter]]\n")
  cat("\n")
  cat("  benchmark      - benchmark class name\n")
  cat("  num-iterations - number of times to execute benchmark, default: 1\n")
  cat("  inner-iter     - number of times the benchmark is executed in an inner loop,\n")
  cat("                   which is measured in total, default: 1\n")
}

run(commandArgs(trailingOnly=TRUE))
