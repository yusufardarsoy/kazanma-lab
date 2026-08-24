THE_ODDS_API_SOURCE <- "The Odds API"
THE_ODDS_API_PUBLIC_URL <- "https://the-odds-api.com/sports-odds-data/sports-apis.html"

the_odds_api_enabled <- function(config) nzchar(config$odds_api_key)

the_odds_api_get <- function(config, endpoint, query = list()) {
  if (!the_odds_api_enabled(config)) stop("ODDS_API_KEY tanımlı değil.")
  request <- httr2::request(paste0(sub("/$", "", config$odds_api_base), endpoint)) |>
    httr2::req_url_query(apiKey = config$odds_api_key, !!!query) |>
    httr2::req_user_agent("Kazanma-Lab/0.4 personal analytics") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3)
  response <- httr2::req_perform(request)
  list(
    body = httr2::resp_body_json(response, simplifyVector = FALSE),
    remaining = suppressWarnings(as.integer(httr2::resp_header(response, "x-requests-remaining"))),
    used = suppressWarnings(as.integer(httr2::resp_header(response, "x-requests-used"))),
    last_cost = suppressWarnings(as.integer(httr2::resp_header(response, "x-requests-last")))
  )
}

the_odds_fixture_id <- function(home_name, away_name) {
  home <- provider_team_lookup(home_name)
  away <- provider_team_lookup(away_name)
  possible <- football_data_catalog() |>
    dplyr::filter(home_team == home$team, away_team == away$team)
  if (nrow(possible) == 1L) possible$fixture_id[[1]] else NA_character_
}

the_odds_timestamp <- function(value, fallback = Sys.time()) {
  value <- provider_chr(value, "")
  parsed <- suppressWarnings(as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (is.na(parsed)) parsed <- fallback
  format(parsed, "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Istanbul")
}

parse_the_odds_rows <- function(raw, fetched_at = Sys.time()) {
  if (length(raw) == 0) return(tibble::tibble())
  purrr::map_dfr(raw, function(event) {
    fixture_id <- the_odds_fixture_id(provider_chr(event$home_team), provider_chr(event$away_team))
    if (is.na(fixture_id)) return(NULL)
    bookmakers <- event$bookmakers %||% list()
    purrr::map_dfr(bookmakers, function(bookmaker) {
      bookmaker_name <- provider_chr(bookmaker$title, provider_chr(bookmaker$key, "Bilinmeyen"))
      snapshot_at <- the_odds_timestamp(bookmaker$last_update, fetched_at)
      purrr::map_dfr(bookmaker$markets %||% list(), function(market) {
        market_key <- provider_chr(market$key, "")
        purrr::map_dfr(market$outcomes %||% list(), function(outcome) {
          outcome_name <- provider_chr(outcome$name, "")
          point <- provider_num(outcome$point)
          market_id <- selection_id <- NA_character_
          if (market_key == "h2h") {
            market_id <- "result"
            selection_id <- if (tolower(outcome_name) == "draw") {
              "draw"
            } else if (normalise_provider_name(outcome_name) == normalise_provider_name(provider_chr(event$home_team))) {
              "home"
            } else if (normalise_provider_name(outcome_name) == normalise_provider_name(provider_chr(event$away_team))) {
              "away"
            } else {
              NA_character_
            }
          } else if (market_key == "totals" && isTRUE(abs(point - 2.5) < 1e-8)) {
            market_id <- "ou_2_5"
            selection_id <- dplyr::case_when(
              tolower(outcome_name) == "over" ~ "over",
              tolower(outcome_name) == "under" ~ "under",
              TRUE ~ NA_character_
            )
          }
          tibble::tibble(
            source = THE_ODDS_API_SOURCE,
            source_url = THE_ODDS_API_PUBLIC_URL,
            fixture_id = fixture_id,
            market_id = market_id,
            selection_id = selection_id,
            bookmaker = bookmaker_name,
            odds = provider_num(outcome$price),
            snapshot_kind = "current",
            snapshot_at = snapshot_at,
            fetched_at = format(fetched_at, "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Istanbul")
          )
        })
      })
    })
  }) |>
    dplyr::filter(!is.na(market_id), !is.na(selection_id), is.finite(odds), odds > 1)
}

parse_the_odds_scores <- function(raw, fetched_at = Sys.time()) {
  if (length(raw) == 0) return(tibble::tibble())
  purrr::map_dfr(raw, function(event) {
    if (!isTRUE(event$completed)) return(NULL)
    home_name <- provider_chr(event$home_team)
    away_name <- provider_chr(event$away_team)
    fixture_id <- the_odds_fixture_id(home_name, away_name)
    if (is.na(fixture_id)) return(NULL)
    scores <- event$scores %||% list()
    score_for <- function(team_name) {
      selected <- purrr::keep(scores, ~ normalise_provider_name(provider_chr(.x$name)) == normalise_provider_name(team_name))
      if (length(selected) != 1L) return(NA_integer_)
      provider_int(selected[[1]]$score)
    }
    home <- provider_team_lookup(home_name)
    away <- provider_team_lookup(away_name)
    tibble::tibble(
      fixture_id = fixture_id,
      match_date = the_odds_timestamp(event$commence_time, fetched_at),
      home_team = home$team,
      away_team = away$team,
      home_goals = score_for(home_name),
      away_goals = score_for(away_name),
      result_source = THE_ODDS_API_SOURCE,
      source_url = THE_ODDS_API_PUBLIC_URL,
      source_published_at = format(fetched_at, "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Istanbul"),
      fetched_at = format(fetched_at, "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Istanbul")
    )
  }) |>
    dplyr::filter(!is.na(home_goals), !is.na(away_goals))
}

the_odds_sync_is_fresh <- function(config, now = Sys.time()) {
  latest <- latest_public_sync_run(config$db_path, THE_ODDS_API_SOURCE)
  if (nrow(latest) == 0 || !latest$status[[1]] %in% c("ok", "cached")) return(FALSE)
  last_time <- parse_model_timestamp(latest$started_at)[[1]]
  !is.na(last_time) && as.numeric(difftime(now, last_time, units = "hours")) < config$odds_refresh_hours
}

sync_the_odds_api <- function(config, now = Sys.time(), force = FALSE) {
  if (!the_odds_api_enabled(config)) {
    return(invisible(list(source = THE_ODDS_API_SOURCE, status = "disabled", results = 0L, odds_rows = 0L, message = "Ücretsiz anahtar ayarlı değil.")))
  }
  initialize_store(config$db_path)
  if (!force && the_odds_sync_is_fresh(config, now)) {
    return(invisible(list(source = THE_ODDS_API_SOURCE, status = "fresh", results = 0L, odds_rows = 0L, message = "8 saatlik ücretsiz kota freni etkin.")))
  }
  started_at <- Sys.time()
  requests_used <- result_count <- odds_count <- 0L
  quota_remaining <- NA_integer_
  request_cost <- function(value, default = 2L) {
    if (length(value) != 1L || is.na(value)) as.integer(default) else as.integer(value)
  }
  tryCatch({
    odds_response <- the_odds_api_get(
      config, paste0("/sports/", config$odds_sport_key, "/odds/"),
      list(regions = config$odds_regions, markets = config$odds_markets, oddsFormat = "decimal", dateFormat = "iso")
    )
    requests_used <- requests_used + request_cost(odds_response$last_cost)
    quota_remaining <- odds_response$remaining
    odds <- parse_the_odds_rows(odds_response$body, now)
    if (nrow(odds) > 0) odds_count <- store_odds_snapshots(odds, config$db_path)

    scores_response <- the_odds_api_get(
      config, paste0("/sports/", config$odds_sport_key, "/scores/"),
      list(daysFrom = 3, dateFormat = "iso")
    )
    requests_used <- requests_used + request_cost(scores_response$last_cost)
    if (length(scores_response$remaining) == 1L && !is.na(scores_response$remaining)) {
      quota_remaining <- scores_response$remaining
    }
    results <- parse_the_odds_scores(scores_response$body, now)
    if (nrow(results) > 0) result_count <- import_postmatch_results(results, config$db_path)
    message <- paste0(odds_count, " oran satırı, ", result_count, " yeni/yenilenen sonuç işlendi; kalan kredi: ", quota_remaining, ".")
    record_public_sync_run(
      config$db_path, THE_ODDS_API_SOURCE, started_at, "ok", requests_used = requests_used,
      results_imported = result_count, odds_rows = odds_count,
      quota_remaining = quota_remaining, message = message
    )
    invisible(list(source = THE_ODDS_API_SOURCE, status = "ok", results = result_count, odds_rows = odds_count, requests_used = requests_used, quota_remaining = quota_remaining, message = message))
  }, error = function(e) {
    record_public_sync_run(
      config$db_path, THE_ODDS_API_SOURCE, started_at, "error", requests_used = requests_used,
      results_imported = result_count, odds_rows = odds_count,
      quota_remaining = quota_remaining, message = conditionMessage(e)
    )
    stop(e)
  })
}
