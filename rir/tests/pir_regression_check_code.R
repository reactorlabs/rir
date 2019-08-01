if (as.numeric(Sys.getenv("FAST_TESTS", unset="0")))
  quit()

tools:::.check_code_usage_in_package(package = "compiler")
