require(ggplot2)
require(reshape2)
require(Hmisc)

args <- commandArgs(trailingOnly = TRUE)

d <- melt(read.csv(args[[1]]), id=c("experiment","benchmark"))
d[[4]] <- as.numeric(d[[4]])

ggplot(d, aes(x=experiment, y=value, color=experiment, group=variable)) +
  scale_x_discrete(labels=1:4) +
  #scale_y_continuous(limits=c(0.7,2.5)) +
  #stat_summary(fun.y = "mean", geom="point") +
  #geom_hline(yintercept=c(0.8,1.2), color="red", size=0.2) +
  #geom_hline(yintercept=c(1), color="red", size=0.5) +
  stat_summary(fun.data = "mean_cl_boot") +
  geom_point(size=0.25, color="black") +
  facet_wrap(~benchmark, scales="free")

ggsave(paste0(args[[1]],".png"), scale=2.5)
