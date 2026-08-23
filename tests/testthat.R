library(testthat)

if (identical(.Platform$OS.type, "windows")) {
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE)))
}

source("R/config.R", encoding = "UTF-8")
source("R/auth.R", encoding = "UTF-8")
source("R/demo_data.R", encoding = "UTF-8")
source("R/model_engine.R", encoding = "UTF-8")
source("R/storage.R", encoding = "UTF-8")

test_dir("tests/testthat", reporter = "summary")
