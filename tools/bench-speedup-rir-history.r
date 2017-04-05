library(ggplot2)
library(Hmisc)

args <- commandArgs(trailingOnly = TRUE)

files <- sort(list.files(path = args[[1]], pattern = "^benchmark.*\\.csv$"))

if (length(files) < 2) {
    cat("Not plotting history, less than 2 records found...\n")
    q()
}

hashes <- sapply(strsplit(sapply(strsplit(files, "_", fixed = TRUE), tail, n = 1), ".", fixed = TRUE), head, n = 1)
files <- file.path(args[[1]], files)

combined <- NULL

for (i in seq_along(files)) {
    d <- read.csv(files[i])
    d <- d[grepl("rir$", d$experiment),]
    d$i <- i
    d$time <- as.numeric(d$time)
    d$version <- hashes[i]
    d$experiment <- NULL
    if (is.null(combined))
        combined <- d
    else
        combined <- rbind.data.frame(combined, d)
}

ggplot(combined, aes(x=reorder(version, i), y=time, group=benchmark)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "smooth") +
  facet_wrap(~benchmark) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("revision") +
  ylab("time [s]")

ggsave(file.path(args[[1]], "speedup_history.pdf"))
ggsave(file.path(args[[1]], "speedup_history.png"), scale=1.5)
