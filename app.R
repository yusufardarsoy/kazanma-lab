options(shiny.maxRequestSize = 20 * 1024^2)

if (identical(.Platform$OS.type, "windows")) {
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE)))
}

required_packages <- c(
  "shiny", "bslib", "shinyjs", "dplyr", "tidyr", "purrr", "ggplot2",
  "scales", "glue", "DBI", "RSQLite", "jsonlite", "httr2", "sodium"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Eksik R paketleri: ", paste(missing_packages, collapse = ", "),
    ". Kurulum için README'deki adımları izleyin."
  )
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(scales)
  library(glue)
})

source_files <- c(
  "R/config.R",
  "R/auth.R",
  "R/demo_data.R",
  "R/super_lig_data.R",
  "R/model_engine.R",
  "R/betting_engine.R",
  "R/storage.R",
  "R/provider_api_football.R",
  "R/provider_public_data.R",
  "R/provider_the_odds_api.R",
  "R/sync_orchestrator.R",
  "R/agent_intelligence.R",
  "R/charts.R",
  "R/ui.R",
  "R/server.R"
)

invisible(lapply(source_files, function(path) source(path, encoding = "UTF-8")))

config <- read_app_config()
initialize_store(config$db_path)

shinyApp(
  ui = app_shell_ui(config),
  server = function(input, output, session) app_server(input, output, session, config)
)
