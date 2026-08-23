read_app_config <- function() {
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
    league_id = as.integer(Sys.getenv("FOOTBALL_LEAGUE_ID", "203")),
    season = as.integer(Sys.getenv("FOOTBALL_SEASON", format(Sys.Date(), "%Y"))),
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
