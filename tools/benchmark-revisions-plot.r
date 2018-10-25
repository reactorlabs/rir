require(ggplot2)
require(Hmisc)

args <- commandArgs(trailingOnly = TRUE)

d <- read.csv(args[[1]])
d[[3]] <- as.numeric(d[[3]])
d <- d[order(d$version),]
d[[1]] <- as.numeric(d[[1]])
d[[1]] <- d[[1]] - max(d[[1]])

ggplot(d, aes(x=version, y=time, group=benchmark)) +
 stat_summary(fun.data = "mean_cl_boot", geom = "smooth") +
 facet_wrap(~benchmark)

ggsave(paste0(args[[1]],".png"), scale=1.5)
