full_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", full_args[grepl("^--file=", full_args)])
script_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[[1]], winslash = "/")) else getwd()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")
setwd(project_root)

required <- c("dplyr", "purrr", "tibble", "DBI", "RSQLite", "jsonlite", "httr2", "glue", "scales")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Eksik R paketleri: ", paste(missing, collapse = ", "))

source("R/config.R", encoding = "UTF-8")
source("R/super_lig_data.R", encoding = "UTF-8")
source("R/model_engine.R", encoding = "UTF-8")
source("R/storage.R", encoding = "UTF-8")
source("R/provider_api_football.R", encoding = "UTF-8")
source("R/provider_public_data.R", encoding = "UTF-8")
source("R/provider_the_odds_api.R", encoding = "UTF-8")
source("R/sync_orchestrator.R", encoding = "UTF-8")

config <- read_app_config()
initialize_store(config$db_path)
result <- auto_sync_all_sources(config)
message("Senkronizasyon tamamlandı: ", result$results, " sonuç, ", result$odds_rows, " oran satırı işlendi. ", result$message)
