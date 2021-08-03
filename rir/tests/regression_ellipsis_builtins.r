# For some reason leak sanitizer crashes on this test...
if (Sys.getenv("ASAN_SYMBOLIZER_PATH", unset="") != "")
  quit()

pkgname <- "stats"
source(file.path(R.home("share"), "R", "examples-header.R"))
acf(lh)
acf(lh)
example(glm, echo = FALSE)
example(glm, echo = FALSE)
