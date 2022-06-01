library(rmarkdown)
for (i in 1:15) {
    startTime <- Sys.time()
    rmarkdown::render("eda_for_highthroughput.Rmd", output_format="md_document", runtime="static", quiet=TRUE)
    endTime <- Sys.time()
    duration <- endTime - startTime
    cat("iter ", i, "\n")
    print(duration)
}
