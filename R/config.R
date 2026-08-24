load_project_env <- function(path = ".env") {
  if (!file.exists(path)) return(invisible(FALSE))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#") & grepl("=", lines, fixed = TRUE)]
  for (line in lines) {
    split_at <- regexpr("=", line, fixed = TRUE)[[1]]
    key <- trimws(substr(line, 1, split_at - 1))
    value <- trimws(substr(line, split_at + 1, nchar(line)))
    value <- sub('^(["\'])(.*)\\1$', "\\2", value)
    if (nzchar(key) && !nzchar(Sys.getenv(key, ""))) do.call(Sys.setenv, stats::setNames(list(value), key))
  }
  invisible(TRUE)
}

read_app_config <- function() {
  load_project_env()
  env <- Sys.getenv("APP_ENV", "development")
  password <- Sys.getenv("APP_PASSWORD", "")
  password_hash <- Sys.getenv("APP_PASSWORD_HASH", "")

  if (identical(env, "production") && !nzchar(password) && !nzchar(password_hash)) {
    stop("Production ortamında APP_PASSWORD veya APP_PASSWORD_HASH zorunludur.")
  }

  list(
    env = env,
    username = Sys.getenv("APP_USERNAME", "arda"),
    password = if (nzchar(password)) password else if (identical(env, "development")) "kazanma-lab" else "",
    password_hash = password_hash,
    db_path = Sys.getenv("KAZANMA_DB_PATH", "data/kazanma.sqlite"),
    football_api_key = Sys.getenv("FOOTBALL_API_KEY", ""),
    football_api_base = Sys.getenv("FOOTBALL_API_BASE", "https://v3.football.api-sports.io"),
    cache_dir = Sys.getenv("KAZANMA_CACHE_DIR", "data/cache"),
    sync_detail_budget = as.integer(Sys.getenv("KAZANMA_SYNC_DETAIL_BUDGET", "8")),
    # Uygulama bilinçli olarak yalnızca 2026-27 Türkiye Süper Ligi'ne kilitlidir.
    league_id = 203L,
    season = 2026L,
    timezone = Sys.getenv("FOOTBALL_TIMEZONE", "Europe/Istanbul")
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

is_demo_environment <- function(config) {
  identical(config$env, "development") && !nzchar(Sys.getenv("APP_PASSWORD", "")) &&
    !nzchar(config$password_hash)
}
