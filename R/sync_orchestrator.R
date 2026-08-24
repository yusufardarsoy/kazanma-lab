auto_sync_all_sources <- function(config, now = Sys.time(), force_public = FALSE, force_odds = FALSE) {
  runs <- list()
  errors <- character()
  run_safely <- function(name, fn) {
    tryCatch(fn(), error = function(e) {
      errors <<- c(errors, paste0(name, ": ", conditionMessage(e)))
      list(source = name, status = "error", results = 0L, odds_rows = 0L, message = conditionMessage(e))
    })
  }

  runs$football_data <- run_safely(
    FOOTBALL_DATA_SOURCE,
    function() sync_public_football_data(config, now, force = force_public)
  )
  runs$the_odds_api <- if (the_odds_api_enabled(config)) {
    run_safely(THE_ODDS_API_SOURCE, function() sync_the_odds_api(config, now, force = force_odds))
  } else {
    list(source = THE_ODDS_API_SOURCE, status = "disabled", results = 0L, odds_rows = 0L, message = "Ücretsiz anahtar ayarlı değil.")
  }
  runs$api_football <- if (api_football_enabled(config)) {
    run_safely("API-Football", function() {
      value <- auto_sync_league(config, now)
      value$source <- "API-Football"
      value$odds_rows <- 0L
      value$message <- paste0(value$fixtures_mapped, " fikstür eşleşti; ", value$results, " sonuç işlendi.")
      value
    })
  } else {
    list(source = "API-Football", status = "disabled", results = 0L, odds_rows = 0L, message = "Ücretsiz anahtar ayarlı değil.")
  }

  statuses <- vapply(runs, function(x) x$status %||% "unknown", character(1))
  useful <- statuses %in% c("ok", "cached", "fresh", "quota_guard")
  if (!any(useful)) stop(paste(errors, collapse = " | "))
  messages <- vapply(runs, function(x) paste0(x$source, ": ", x$message), character(1))
  invisible(list(
    status = if (length(errors)) "partial" else "ok",
    sources = runs,
    results = sum(vapply(runs, function(x) as.integer(x$results %||% 0L), integer(1))),
    odds_rows = sum(vapply(runs, function(x) as.integer(x$odds_rows %||% 0L), integer(1))),
    message = paste(messages, collapse = " | ")
  ))
}
