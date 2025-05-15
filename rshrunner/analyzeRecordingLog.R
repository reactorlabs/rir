#convolution convolve

library(tidyverse)
library(digest)


logsFolder <- "/home/skrynski/splitFeedback/benchsuitelog"
#fullPath <- file.path(logsFolder, "Replay_Ctxs", "RealThing", "volcano.csv")
fullPath <- "/home/skrynski/splitFeedback/compare/split_recording.csv"


#sdata <- read_csv(fullPath, col_names = TRUE, col_types= "cccccccccccccc")
sdata <- read_csv(fullPath, col_names = TRUE)

compilations <- filter(sdata, type == "Compilation", fun != "osr", fun != "FUN")


osrCompilations <- filter(sdata, type == "CompilationStart", stringr::str_detect(reason, "OSRLoop")) %>% summarise(idxComp=idx+1) %>% pull(idxComp)
compilations <- compilations %>% filter(!(idx %in% osrCompilations))


#compilations %>% arrange(fun,ctx) %>% view()


funWithMoreThanOneContext <- compilations %>% group_by(fun) %>% reframe(distCtx=n_distinct(ctx)) %>% filter(distCtx > 1) %>% pull(fun)



compilationsToAnalyze <- compilations %>% filter(fun %in% funWithMoreThanOneContext)                                               
compilationsToAnalyze <- compilationsToAnalyze %>% distinct(fun, ctx, .keep_all = TRUE)                                               


compilationsToAnalyze %>%
  select(idx,type,fun,env,ctx,bitcode_len, pir_len,speculative) %>%
  mutate(digest2int(speculative)) %>%
  arrange(fun,ctx,speculative) %>% view()

