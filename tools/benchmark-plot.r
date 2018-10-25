require(ggplot2)
require(Hmisc)

args <- commandArgs(trailingOnly = TRUE)

d <- read.csv(args[[1]])
d[[3]] <- as.numeric(d[[3]])

ggplot(d, aes(x=experiment, y=time, color=experiment, group=benchmark)) +
  scale_x_discrete(labels=1:4) +
  stat_summary(fun.data = "mean_cl_boot") +
  geom_point(size=0.25, color="black") +
  facet_wrap(~benchmark, scales="free")

ggsave(paste0(args[[1]],".png"), scale=1.5)
