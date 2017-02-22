require(ggplot2)
require(reshape2)
require(Hmisc)

args <- commandArgs(trailingOnly = TRUE)

d <- melt(read.csv(args[[1]]), id=c("version","benchmark"))
d[[4]] <- as.numeric(d[[4]])

ggplot(d, aes(x=version, y=value, color=benchmark, group=variable)) +
 stat_summary(fun.data = "mean_cl_boot", geom = "smooth") +
 facet_wrap(~benchmark)

ggsave(paste0(args[[1]],".png"), scale=1.5)
