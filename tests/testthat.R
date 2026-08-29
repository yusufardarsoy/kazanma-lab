library(testthat)
library(shiny)
library(bslib)
library(shinyjs)

if (identical(.Platform$OS.type, "windows")) {
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE)))
}

source("R/config.R", encoding = "UTF-8")
source("R/auth.R", encoding = "UTF-8")
source("R/demo_data.R", encoding = "UTF-8")
source("R/super_lig_data.R", encoding = "UTF-8")
source("R/model_engine.R", encoding = "UTF-8")
source("R/betting_engine.R", encoding = "UTF-8")
source("R/storage.R", encoding = "UTF-8")
source("R/provider_api_football.R", encoding = "UTF-8")
source("R/provider_public_data.R", encoding = "UTF-8")
source("R/provider_the_odds_api.R", encoding = "UTF-8")
source("R/sync_orchestrator.R", encoding = "UTF-8")
source("R/agent_intelligence.R", encoding = "UTF-8")
source("R/charts.R", encoding = "UTF-8")
source("R/ui.R", encoding = "UTF-8")
source("R/server.R", encoding = "UTF-8")

test_dir("tests/testthat", reporter = "summary")
