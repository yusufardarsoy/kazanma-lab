api_football_enabled <- function(config) nzchar(config$football_api_key)

api_football_get <- function(config, endpoint, query = list()) {
  if (!api_football_enabled(config)) stop("FOOTBALL_API_KEY tanımlı değil.")

  request <- httr2::request(paste0(sub("/$", "", config$football_api_base), endpoint)) |>
    httr2::req_headers(`x-apisports-key` = config$football_api_key) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3)

  response <- httr2::req_perform(request)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  if (!is.null(body$errors) && length(body$errors) > 0) {
    stop("API-Football hatası: ", jsonlite::toJSON(body$errors, auto_unbox = TRUE))
  }
  body$response
}

fetch_upcoming_fixtures <- function(config, next_n = 15L) {
  raw <- api_football_get(config, "/fixtures", list(
    league = config$league_id,
    season = config$season,
    `next` = next_n,
    timezone = config$timezone
  ))
  if (length(raw) == 0) return(tibble::tibble())

  purrr::map_dfr(raw, function(x) {
    tibble::tibble(
      fixture_id = as.character(x$fixture$id),
      kickoff = as.POSIXct(x$fixture$date, tz = config$timezone),
      home_team = x$teams$home$name,
      away_team = x$teams$away$name,
      league = x$league$name,
      status = x$fixture$status$short
    )
  })
}

fetch_fixture_snapshot <- function(config, fixture_id) {
  fixture <- api_football_get(config, "/fixtures", list(id = fixture_id, timezone = config$timezone))
  prediction <- api_football_get(config, "/predictions", list(fixture = fixture_id))
  lineups <- api_football_get(config, "/fixtures/lineups", list(fixture = fixture_id))
  injuries <- api_football_get(config, "/injuries", list(fixture = fixture_id, timezone = config$timezone))
  list(fixture = fixture, provider_prediction = prediction, lineups = lineups, injuries = injuries)
}
