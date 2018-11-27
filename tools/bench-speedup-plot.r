library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)

d <- read.csv(args[[1]])
d$time <- as.numeric(d$time)

experiments <- length(unique(d$experiment))
benchmarks <- length(unique(d$benchmark))

d <- setNames(aggregate(d$time, list(d$experiment, d$benchmark), mean),
              c("experiment", "benchmark", "time"))

d <- reshape(d,
             v.names = "time",
             timevar = "experiment",
             idvar = "benchmark",
             direction = "wide")

for (col in (experiments:1)+1)
  d[[col]] <- d[[2]] / d[[col]]

d <- reshape(d)

colnames(d)[colnames(d) == "time"] <- "speedup"

ggplot(d, aes(x = experiment,
              y = speedup,
              color = experiment,
              fill = experiment,
              group = benchmark)) +
  scale_x_discrete(labels = 1:4) +
  geom_bar(stat = "identity", color = "black", width = 0.8) +
  theme_minimal() +
  facet_wrap(~benchmark, scales = "free")

ggsave(paste0(args[[1]], ".pdf"))
ggsave(paste0(args[[1]], ".png"), scale=1.5)
