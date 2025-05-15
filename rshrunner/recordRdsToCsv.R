args = commandArgs(trailingOnly=TRUE)
fromRDS <- args[[1]]
toCSV <- args[[2]]


source('/home/skrynski/splitFeedback/rir-split/replayer.r')
# s <- recordings.csv(fromRDS)
# cat(s, file=toCSV)

recordings.csv(fromRDS,toCSV)
