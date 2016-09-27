require(ggplot2)
require(reshape2)
require(Hmisc)

experiments <- c("1 R_JIT_ENABLE=3 tools/R", "2 R_JIT_ENABLE=0 tools/R", "3 R_JIT_ENABLE=3 $PLAIN_R", "4 R_JIT_ENABLE=2 $PLAIN_R", "5 R_JIT_ENABLE=0 $PLAIN_R", "6 tools/R enableJit(level=1, type='sticky')", "7 tools/R enableJit(level=2, type='sticky')", "8 tools/R enableJit(type='force')")

process <- function(name) {
  raw <- readLines(name)
  bench <- 0
  inpos <- 1
  outpos <- 1
  processed <- c(1:length(raw))
  while (inpos  <= length(raw)) {
    bench <- bench + 1
    # label
    processed[outpos] <- raw[inpos]; outpos <- outpos+1; inpos <- inpos+1;
    for (i in 1:length(experiments)) {
      if (length(grep(";;", raw[inpos]))) {
        processed[outpos] <- ""; outpos <- outpos+1
      } else {
        if (!length(grep(":", raw[inpos]))) {
          stop("malformed input")
        } else {
          processed[outpos] <- raw[inpos]; outpos <- outpos+1; inpos <- inpos+1
          if (length(grep(";;", raw[inpos]))) {
            inpos <- inpos+1
          } else {
            stop("malformed input")
          }
        }
      }
      if (inpos > length(raw)) {
        warning("premature end of input")
        bench <- bench - 1
        break;
      }
    }
  }

  processed <- processed[1:(bench*(1+length(experiments)))]
  data <- matrix(processed, ncol=bench)
  dat <- matrix(as.numeric(lapply(data[-1,], function(p)
      (function(p)
        if (length(p) != 2) NA
        else as.integer(p[[1]])*60+as.numeric(p[[2]]))(
          strsplit(p,":")[[1]]))), ncol=bench)
  for (i in 1:ncol(dat))
    dat[,i] <- dat[,i] / dat[1,i]

  dat <- matrix(as.numeric(
          lapply(dat, function(p) if (is.na(p) || p > 2) NA else p)),
                ncol=bench)

  datf <- melt(dat)
  datf$cmd <- experiments
  datf$bench <- data[1,][datf$Var2]

  datf
}

d <- data.frame()
for (f in list.files(pattern="benchrun*"))
  d <- rbind(d, process(f))

ggplot(d, aes(x=cmd,y=value,color=cmd)) +
  scale_x_discrete(labels=1:length(experiments)) +
  scale_y_continuous(limits=c(0.5,1.8)) +
  #stat_summary(fun.y = "mean", geom="point") +
  stat_summary(fun.data = "mean_cl_boot") +
  geom_point(size=0.2) +
  facet_wrap(~bench)

ggsave("benchout.png")
