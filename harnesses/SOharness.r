serializing <- if (Sys.getenv("SERIALIZER_RUN",0) != "0") TRUE else FALSE;
deserializing <- if (Sys.getenv("DESERIALIZER_RUN",0) != "0") TRUE else FALSE;
warmupDiff <- if (Sys.getenv("WARMUPDIFF",0) != "0") TRUE else FALSE;

verifyResult <- function(x, ...) {
    UseMethod("verifyResult", x)
}

verifyResult.default <- function(result, benchmarkParameter) {
    TRUE
}

innerBenchmarkLoop <- function(x, ...) {
    UseMethod("innerBenchmarkLoop", x)
}

innerBenchmarkLoop.default <- function(class, benchmarkParameter, innerIterations) {
    for (i in 1:innerIterations) {
        if (!verifyResult(execute(benchmarkParameter), benchmarkParameter)) {
            return(FALSE)
        }
    }
    return(TRUE)
}

doRuns <- function(name, iterations, benchmarkParameter, innerIterations) {
    total <- 0
    class(name) <- tolower(name)
    path <- paste("/opt/bitcodes/", gsub("/","_",name), sep ="")

    # Serializer run
    if (serializing) {
        dir.create(path)
        Sys.setenv(PIR_SERIALIZE_PREFIX = path)
    }

    # Deserializer run
    if (deserializing) {
        Sys.setenv(PIR_DESERIALIZE_PREFIX = path)
        f.loadBitcodes()
    }

    # Show warmup differently
    if (warmupDiff) {
        startTime =  Sys.time()
        for (i in 1:5) {
        if (serializing) f.startSerializer()
        if (!innerBenchmarkLoop(name, benchmarkParameter, innerIterations)) {
            stop("Benchmark failed with incorrect result")
        }
        if (serializing) f.stopSerializer()
        }
        endTime <- Sys.time()
        runTime = (as.numeric(endTime) - as.numeric(startTime)) * 1000000
        cat("Warmup: ",runTime, "\n")
    }

    for (i in 1:iterations) {
        startTime <- Sys.time()
        if (serializing) f.startSerializer()
        if (!innerBenchmarkLoop(name, benchmarkParameter, innerIterations)) {
            stop("Benchmark failed with incorrect result")
        }
        if (serializing) f.stopSerializer()
        endTime <- Sys.time()
        runTime <- (as.numeric(endTime) - as.numeric(startTime)) * 1000000

        cat(name, ": iterations=1 runtime: ", round(runTime), "us\n", sep = "")
        total <- total + runTime
    }
    total
}

run <- function(args) {
    if (length(args) < 3 || 4 < length(args))
        stop(printUsage())

    name <- args[[1]]
    numIterations <- strtoi(args[[2]])
    benchmarkParameter <- strtoi(args[[3]])
    innerIterations <- if (length(args) > 3) strtoi(args[[4]]) else 1

    source(file.path(".", paste(tolower(name), ".r", sep="")))

    total <- as.numeric(doRuns(name, numIterations, benchmarkParameter, innerIterations));
    cat(name, ": ",
        "iterations=", numIterations, "; ",
        "average: ", round(total / numIterations), " us; ",
        "total: ", round(total), "us\n\n", sep="")
    #cat("Total runtime: ", total, "us\n\n", sep="")
}

printUsage <- function() {
    cat("harness.r benchmark num-iterations benchmark-parameter [inner-iter]\n")
    cat("\n")
    cat("  benchmark           - benchmark class name\n")
    cat("  num-iterations      - number of times to execute benchmark\n")
    cat("  benchmark-parameter - size of the benchmark problem\n")
    cat("  inner-iter          - number of times the benchmark is executed in an inner loop,\n")
    cat("                        which is measured in total, default: 1\n")
}

run(commandArgs(trailingOnly=TRUE))
