


executeBenchmark <- function(path, size = FALSE, jit = FALSE) {
    source(path, local = TRUE)
    if (jit)
        ctime = system.time(compile())
    else
        ctime = c(user = NA, system = NA, ellapsed = NA, NA, NA)
    #cat(jit.llvm(binarytrees_naive))
    if (missing(size))
        etime <- system.time(execute(size))
    else
        etime <- system.time(execute())
    # returns the time of compilation and time of execution of the benchmark
    list(name = path, compile = ctime, execute = etime)
}

executeBenchmarks <- function(path, jit, recursive = TRUE) {
    results = c()
    for (file in list.files(path, pattern = "\\.r$", recursive = recursive)) {
        cat(paste(file, "\n"))
        results[[length(results) + 1]] <- executeBenchmark(paste(path, "/", file, sep=""), jit)
    }
    for (x in results)
        cat(paste(x$execute[[1]], x$name, "\n"))
}



info <- function(..., override = FALSE) {
    if (verbose || override)
        cat(...)
}


# n times execures and compiles (if selected) the given benchmark. Execution failure checks should be done by the caller of this function.
command.executeSingleBenchmark <- function(file, n, compile) {
    compileTime <- c()
    executionTime <- c()
    while (n > 0) {
        e = new.env()
        source(file, local = e)
        if (compile)
            append(compileTime, system.time(rjit.compileEnvironment(e)))
        capture.output({
            executionTime[[length(executionTime) + 1]] <- system.time(e$execute())
        })
        info(".")
        n = n - 1
    }
    if (length(compileTime) == 0)
        compileTime <- NULL
    list(file = file, compilationTime = compileTime, executionTime = executionTime)
}




# Executes test from given test root and returns the results as a data frame with additional information stored in its attributes
#
# testroot is the test root, which is scanned recursively for *.r files which are then compiled (if compile == TRUE) and executed, both n times, if specified.
command.execute <- function(testroot, n, compile) {
    results = c()
    attributes(results)$date <- Sys.time()
    totalTime = system.time({
    for (file in list.files(testroot, pattern = "\\.r$", recursive = TRUE)) {
        info(file, override = TRUE)
        file = paste(testroot, "/", file, sep="")
        results[[length(results) + 1]] <- tryCatch({
                r <- command.executeSingleBenchmark(file, n, compile)
                info(mean(r$executionTime[[1]]))
                r
            },
            error = function(e) { info("FAIL"); list(file = file, compilationTime = NULL, executionTime = NULL, error = e) }
        )
        info("\n", override = TRUE)
    }
    })
    attributes(results)$gitBranch <- getGitBranch()
    attributes(results)$gitCommit <- getGitCommit()
    attributes(results)$arguments <- list(verbose = verbose, testroot = testroot, n = n, compile = compile)
    attributes(results)$system <- getCPUInfo()
    attributes(results)$totalTime <- totalTime
    results
}

command.store <- function(runInfo, name) {
    attributes(runInfo)$name = name
    attr = attributes(runInfo)
    filename = paste(paste(name, attr$gitBranch, attr$gitCommit, sep = "_"), ".rds", sep = "")
    if (file.exists(filename))
        info(paste("File", filename, "already exists and will be overwritten.\n"), override = TRUE)
    saveRDS(runInfo, filename)
}


getExecutionTimes <- function(runInfo) {
    times = c()
    for (v in runInfo) {
        name = strsplit(v$file, "/")[[1]]
        name = tail(name, 1)
        name = strsplit(name, "[.]")[[1]][[1]]
        t = mean(sapply(v$executionTime, function(x) x[[1]]))
        times[[name]] = t
    }
    times
}

getCompilationTimes <- function(runInfo) {
    if (runInfo[[1]]$compilationTime == NULL) {
        NULL
    } else {
        times = c()
        for (v in runInfo) {
            name = strsplit(v$file, "/")[[1]]
            name = tail(name, 1)
            name = strsplit(name, "[.]")[[1]][[1]]
            t = mean(sapply(v$executionTime, function(x) x[[1]]))
            times[[name]] = t
        }
        times
    }
}

# returns the execution time series, which is useful for boxplots graphs of execution times. If the current benchmark or test has failed, NA is returned in the current slot. Returns a list of named vectors of execution times.
getExecutionTimeSeries <- function(runInfo) {
    times = list()
    for (v in runInfo) {
        name = v$file
        if (is.null(v$executionTime))
            t = NA
        else
            t = sapply(v$executionTime, function(x) x[[1]])
        times[[name]] = t
    }
    oldNames = names(times)
    attributes(times) = attributes(runInfo)
    names(times) = oldNames
    attributes(times)$title = "Execution Times"
    attributes(times)$unit = "[s]"
    times
}

# returns the compilation time series, which is useful for boxplots graphs of execution times. If the current benchmark has not been compiled, returns NA in the corresponding slot. Returns a list of named vectors of compilation times.
getCompilationTimeSeries <- function(runInfo) {
    times = list()
    for (v in runInfo) {
        name = v$file
        if (is.null(v$compilationTime))
            t = NA
        else
            t = sapply(v$compilationTime, function(x) x[[1]])
        times[[name]] = t
    }
    oldNames = names(times)
    attributes(times) = attributes(runInfo)
    names(times) = oldNames
    attributes(times)$title = "Compilation Times"
    attributes(times)$unit = "[s]"
    times
}

getSeriesMeans <- function(series) {
    result = sapply(series, mean)
}

calculateSpeedup <- function(series1, series2) {
    result = c()
    for (n in names(series1))
        # this speedup calculation is perhaps statistically wrong (?)
        result[[n]] = mean(series2[[n]] / series1[[n]])
    attributes(result) = list(
        name = "speedup",
        title = "Speedup",
        unit = "[%]",
        gitBranch = "",
        gitCommit = "",
        first = attributes(series1),
        second = attributes(series2),
        names = names(series1)
    )
    result
}


#plotSpeedup <- function(speedup, colorFaster = "#0000ff", colorSlower = "#ff0000") {
#    png(paste(paste(attr$title, attr$name, attr$gitBranch, attr$gitCommit, sep="_"),".png", sep=""), width = width, height = height)
#    orig = par()
#    barplot(zmean - 1, ylim = c(-1, max(zmean) - 1), col = sapply(zmean, function(x) if (x >= 1) "#0000ff" else "#ff0000"))
#}






library(utils)
library(grDevices)
library(graphics)

width = 1366L
height = 768L
verbose = TRUE

# returns name of the current branch
getGitBranch <- function() {
    strsplit(system2("git", "status", stdout = TRUE)[[1]], " ")[[1]][[3]]
}

# returns the latest commit of the current branch
getGitCommit <- function() {
    system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE)
}

# get CPU information
getCPUInfo <- function() {
    system2("lscpu", stdout = TRUE)
}

# gets the list of older revisions
getGitCommitList <- function() {
    substr(system2("git", c("rev-list", "--all", "HEAD"), stdout = TRUE), 1, 7)
}



simplifyNames <- function(longNames) {
    sapply(longNames, function(x) {
        x = strsplit(x, "/")[[1]]
        x = tail(x, 1)
        strsplit(x, "[.]")[[1]][[1]]
    }, USE.NAMES = FALSE)
}



runTestsInRoot<- function(testroot, n, compile) {
    results = c()
    for (file in list.files(testroot, pattern = "\\.r$", recursive = TRUE)) {
        info(file, override = TRUE)
        file = paste(testroot, "/", file, sep="")
        results[[length(results) + 1]] <- tryCatch({
                r <- command.executeSingleBenchmark(file, n, compile)
                info(mean(r$executionTime[[1]]))
                r
            },
            error = function(e) { info("FAIL"); list(file = file, compilationTime = NULL, executionTime = NULL, error = e) }
        )
        info("\n", override = TRUE)
    }
    print(results)
    results
}


runTests <- function(roots, name, n, compile) {
    results = c()
    attributes(results)$date <- Sys.time()
    totalTime = system.time({
    for (testroot in roots)
        results = c(results, runTestsInRoot(testroot, n, compile))
    })
    attributes(results)$gitBranch <- getGitBranch()
    attributes(results)$gitCommit <- getGitCommit()
    attributes(results)$system <- getCPUInfo()
    attributes(results)$totalTime <- totalTime
    results
}

# executes the test and stores its result in the given file, if name is specified
test <- function(args) {
    n = 1
    name = "unnamed"
    compile = FALSE
    roots = c()
    i = 1
    # compile the arguments crudely
    while (i <= length(args)) {
        arg = args[[i]]
        if (arg == "n") {
            i = i + 1
            n = as.integer(args[[i]])
        } else if (arg == "compile") {
            compile = TRUE
        } else if (arg == "name") {
            i = i + 1
            name = args[[i]]
        } else {
            roots = c(roots, arg)
        }
        i = i + 1
    }
    # now run the tests
    results = runTests(roots, name, n, compile)
    attributes(results)$name = name
    attributes(results)$arguments = args
    # store the tests under their name and git branch
    attr = attributes(results)
    filename = paste(paste(name, attr$gitBranch, attr$gitCommit, sep = "_"), ".rds", sep = "")
    if (file.exists(filename))
        info(paste("File", filename, "already exists and will be overwritten.\n"), override = TRUE)
    saveRDS(results, filename)
}

plotTimesGraph <-function(series, means, boxes, width, height, barsColor, legendColor) {
    attr = attributes(series)
    unit = attr$unit
    title = attr$title
    png(paste(paste(attr$title, attr$name, attr$gitBranch, attr$gitCommit, sep="_"),".png", sep=""), width = width, height = height)
    orig = par()
    par(xaxt="n", mar = c(9, 4, 4, 0) + 0.1)
    if (means) {
        avg = getSeriesMeans(series)
        boxplot(series, col = "transparent", border = "transparent")
        lines(avg, col = barsColor, type = "h", lwd = width * 0.8 / length(avg), lend = 1)
        par(new = T)
    }
    if (boxes)
        boxplot(series, main = title, ylab = unit)
    n = simplifyNames(names(series))
    par(xaxt="s")
    axis(1, at=seq(1, length(n), by=1), labels = FALSE)
    text(seq(1, length(n), by=1) + 0.5, par("usr")[3], labels = paste(n, "     "), srt = 45, adj = 1, xpd = TRUE, col = legendColor)
    par(orig)
    dev.off()
}

plotTimes <- function(args) {
    names = c()
    means = TRUE
    boxes = TRUE
    barsColor = "#d0d0d0"
    legendColor = "#000000"
    # again deal with the arguments
    i = 1
    while (i <= length(args)) {
        arg = args[[i]]
        if (arg == "nomeans") {
            means = FALSE
        } else if (arg == "noboxes") {
            boxes = FALSE
        } else if (arg == "bars") {
            i = i + 1
            barsColor = args[[i]]
        } else if (arg == "legend") {
            i = i + 1
            legendColor = args[[i]]
        } else {
            names = c(names, arg)
        }
        i = i + 1
    }
    if (barsColor == "rainbow")
        barsColor = rainbow(length(series))
    if (legendColor == "rainbow")
        legendColor = rainbow(legend(series))
    # and now print the graphs
    for (name in names) {
        data = readRDS(name)
        times = getExecutionTimeSeries(data)
        plotTimesGraph(times, means, boxes, width, height, barsColor, legendColor)
        # check if we have compilation times data
        times = getCompilationTimeSeries(data)
        if (! is.na(times[[1]]))
           plotTimesGraph(times, means, boxes, width, height, barsColor, legendColor)
    }
}

calculateSpeedup <- function(series1, series2) {
    result = c()
    for (n in names(series1))
        # this speedup calculation is perhaps statistically wrong (?)
        result = c(result, mean(series2[[n]] * 1.2 / series1[[n]]))
    attributes(result) = list(
        name = "speedup",
        title = "Speedup",
        unit = "[%]",
        gitBranch = "",
        gitCommit = "",
        first = attributes(series1),
        second = attributes(series2),
        names = names(series1)
    )
    result
}

plotSpeedup <- function(speedup, fasterColor, slowerColor) {
    attr = attributes(speedup)
    png(paste(paste(attr$title, attr$name, attr$gitBranch, attr$gitCommit, sep="_"),".png", sep=""), width = width, height = height)
    orig = par()
    par(xaxt="n", yaxt="n", mar = c(9, 4, 4, 0) + 0.1)
    data = as.double(speedup - 1)
    present = lapply(data, function(x) if (is.na(x)) NA else 0)
    ylim = c(-1, max(max(data, na.rm = TRUE), 1))
    boxplot(present, ylim = ylim, col = "transparent", border = "transparent")
    lines(data, col = sapply(speedup, function(x) if (is.na(x)) { "#000000"; } else if (x >= 1) fasterColor else slowerColor), type = "h", lwd = width * 0.8 / length(data), lend = 1)
    par(new = T)
    boxplot(present, main = attr$title, ylab = "%")

   #barplot(data, ylim = ylim, col = sapply(speedup, function(x) if (is.na(x)) { "#000000"; } else if (x >= 1) fasterColor else slowerColor))
    n = simplifyNames(names(speedup))
    par(yaxt="s", xaxt="s")
    axis(1, at=seq(1, length(n), by=1), labels = FALSE)
    text(seq(1, length(n), by=1) + 0.5, par("usr")[3], labels = paste(n, "      "), srt = 45, adj = 1, xpd = TRUE)
    axis(2, at=seq(-1, ylim[[2]], by=1), labels = seq(0, ylim[[2]] + 1, by = 1))
    par(orig)
    dev.off()
}

plotComparison <- function(args) {
    names = c()
    fasterColor = "#00ff00"
    slowerColor = "#ff0000"
    i = 1
    # arguments
    while (i <= length(args)) {
        arg = args[[i]]
        if (arg == "faster") {
            i = i + 1
            fasterColor = args[[i]]
        } else if (arg == "slower") {
            i = i + 1
            slowerColor = args[[i]]
        } else {
            names = c(names, arg)
        }
        i = i + 1
    }
    if (length(names) != 2)
        stop("Only two runs can be compared at once")
    # load the runs info
    data1 = readRDS(names[[1]])
    data2 = readRDS(names[[2]])
    executionSpeedup = calculateSpeedup(getExecutionTimeSeries(data1), getExecutionTimeSeries(data2))
    plotSpeedup(executionSpeedup, fasterColor, slowerColor)
    compilationSpeedup = calculateSpeedup(getCompilationTimeSeries(data1), getCompilationTimeSeries(data2))
    if (! is.na(compilationSpeedup))
        plotSpeedup(compilationSpeedup, fasterColor, slowerColor)
}


plotSeries = function(args) {
    names = c()
    i = 1
    # arguments
    while (i <= length(args)) {
        arg = args[[i]]
        {
            names = c(names, arg)
        }
        i = i + 1
    }
    if (length(names) == 0)
        stop("At least one series name must be specified")
    branch = getGitBranch()
    for (name in rev(names)) {
        # get all the data
        data = list()
        for (id in getGitCommitList()) {
            fname = paste(paste(name, branch, id, sep = "_"), ".rds", sep = "")
            if (file.exists(fname))
                data[[length(data) + 1]]  = readRDS(fname)
        }
        # now for each benchmark, get the series data out of it
        series = list()
        for (i in seq(1, length(data))) { # for all runs
            for (j in data[[i]]) { # for all benchmarks
                n = j$file
                series[[n]][[i]] = mean(sapply(j$executionTime, function(x) x[[1]]))
            }
        }
        print(series)
        # and now, for each series, print the result
        names(series) = simplifyNames(names(series))
        runs = sapply(data, function(x) attributes(x)$gitCommit)
        for (n in names(series)) {
            png(paste(paste(name,n,sep="_"), ".png", sep=""), width = width, height = height)
            plot(series[[n]],type="l",xaxt="n", main = n, xlab = "Git commits", ylab="Execution time [s]")
            axis(1,at=seq(1, length(runs)),labels=runs)
            dev.off()
        }
    }
}




processCommonArguments <- function(args) {
    result = c()
    i = 2 # skip command
    while ( i <= length(args)) {
        arg = args[[i]]
        if (arg == "verbose") {
            verbose <<- TRUE
        } else if (arg == "width") {
            i = i + 1
            width = as.integer(args[[i]])
        } else if (arg == "height") {
            i = i + 1
            height = as.integer(args[[i]])
        } else {
            result = c(result, arg)
        }
        i = i + 1
    }
    result
}

help = "
RJIT benchmark & test regression suite
--------------------------------------

Usage benchmarks.r command args

Where commands are one of the following:

test n 7 name blabla compile {root}

times {names}

comparison name1 name2 faster slower

series {names} last 7
"



error <- function(message) {
    cat(help)
    stop(message)
}




args = commandArgs(trailingOnly = TRUE)

args = c("test", "name", "rjit", "n", "1", "compile", "shootout")
#args = c("times", "baseline.rds")

#args = c("comparison", "test_peta_1673921.rds", "test_peta_1673921.rds")
#args = c("series", "test")
print(args)
if (length(args) < 1)
    error("Command argument missing")
command = args[[1]]
args = processCommonArguments(args)
switch(command,
    test = test(args),
    times = plotTimes(args),
    comparison = plotComparison(args),
    series = plotSeries(args),
    error(paste("Unknown command", command))
)















#x <<- command.execute("shootout", 10, FALSE)
#command.store(x, "test_all_r")
#z <<- getExecutionTimeSeries(x)

if (FALSE) {

x = readRDS("~/test.rds")
attributes(x)$name="test"
times = getExecutionTimeSeries(x)
plotSeries(times)

y = readRDS("~/testr.rds")
t2 = getExecutionTimeSeries(y)
s = calculateSpeedup(times, t2)
print(times)
print(t2)
plotSeries(s)

}

#cat("rjit benchmarks...\n")
#dyn.load("../build/librjit.so")
#source("../rjit/R/rjit.R")
#cat("  initialized rjit libraries...\n")

#executeBenchmarks("shootout", jit = FALSE, recursive = TRUE)

#print(executeBenchmark("shootout/binarytrees/binarytrees-2.r",10, FALSE))

