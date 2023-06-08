#! /usr/bin/env Rscript

library(stringr)
library(dplyr)
library(purrr)
library(readr)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    stop("Provide a path to the files to profile")
}

profile_dir <- if (length(args) < 2) {
    "profiles"
} else {
    args[[2]]
}

rfiles_dir <- args[[1]]

r_dir <- Sys.getenv("R_DIR", unset = "../build/bin")


rfiles_path <- list.files(rfiles_dir, pattern = "*\\.(R|r)", full.names = TRUE)

get_metrics <- function(rfile_path) {
    # warn = FALSe to ignore
    lines <- readLines(rfile_path, warn = FALSE)

    list(nb_lines = length(lines), nb_loops = sum(str_detect(lines, "while|for|repeat")))
}


process_profile <- function(rfile_path, profile_dir) {
    # profile the file the "incomplete final line found" warning
    profile_path <- file.path(profile_dir, paste0("profile-phase-", basename(tools::file_path_sans_ext(rfile_path))))

    r_path <- file.path(r_dir, "R")

    args <- c("-q", paste0("-f ", rfile_path))
    res <- system2(r_path, args,
        stdout = FALSE,
        env = c(
            "PIR_MEASURE_COMPILER=1", "PIR_MEASURE_COMPILER_BACKEND=1",
            paste0("PIR_MEASURING_LOGFILE=", profile_path)
        )
    )

    if (res != 0) { # the command failed, just ignore
        cat("Error when profiling: ", rfile_path, "\n")
        return(tibble())
    }

    # get some metrics about the file
    metrics <- get_metrics(rfile_path)

    # Read the result of profiling
    lines <- readLines(profile_path)

    if (length(lines) < 7) {
        return(tibble())
    }

    parsed_line <- str_match(lines[[6]], "\\s*Timers \\((\\d+\\.?\\d*) (secs|mins) [^\\)]*\\):")
    sec_convert <- if (parsed_line[, 3] == "mins") {
        60
    } else {
        1
    }
    total_measured_time <- as.double(parsed_line[, 2]) * sec_convert

    # keep only lines with the phases
    lines <- lines[8:length(lines) - 1]

    df <- tibble(
        filename = rfile_path,
        nb_lines = metrics$nb_lines,
        nb_loops = metrics$nb_loops,
        total_measured_time = total_measured_time
    )
    for (line in lines) {
        res <- str_match(line, "\\s*([^:\\s]+): ([^\\t]+)\\t(\\d+\\.?\\d*(?:e-\\d+)?)")

        phase_name <- str_replace_all(res[, 3], " ", "") # remove whitespaces to make it easier to handle later in R
        df[, phase_name] <- as.double(res[, 4])
        # percentages
        df[, paste0(phase_name, "-pc")] <- as.double(res[, 4]) / total_measured_time * 100
    }
    df
}

extracted <- map(rfiles_path, process_profile, profile_dir, .progress = TRUE)

profiles <- bind_rows(extracted)


write_csv(profiles, "profiles.csv")
