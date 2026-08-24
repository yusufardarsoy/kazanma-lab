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

config <- read_app_config()
initialize_store(config$db_path)
if (!api_football_enabled(config)) {
  message("FOOTBALL_API_KEY ayarlı değil. Veri görevi değişiklik yapmadan kapandı.")
  quit(save = "no", status = 2L)
}

result <- auto_sync_league(config)
message(
  "Senkronizasyon tamamlandı: ", result$fixtures_mapped, " fikstür eşleşti, ",
  result$results, " sonuç hafızada, ", result$requests_used, " API isteği kullanıldı."
)
