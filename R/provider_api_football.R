api_football_enabled <- function(config) nzchar(config$football_api_key)

API_FOOTBALL_SOURCE <- "API-Football"

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

provider_chr <- function(value, default = NA_character_) {
  if (is.null(value) || length(value) == 0) return(default)
  out <- as.character(value[[1]])
  if (!nzchar(out)) default else out
}

provider_int <- function(value, default = NA_integer_) {
  if (is.null(value) || length(value) == 0) return(default)
  out <- suppressWarnings(as.integer(value[[1]]))
  if (is.na(out)) default else out
}

provider_num <- function(value, default = NA_real_) {
  if (is.null(value) || length(value) == 0) return(default)
  out <- suppressWarnings(as.numeric(value[[1]]))
  if (is.na(out)) default else out
}

normalise_provider_name <- function(value) {
  value <- as.character(value)
  ascii <- suppressWarnings(iconv(value, from = "UTF-8", to = "ASCII//TRANSLIT"))
  ascii[is.na(ascii)] <- value[is.na(ascii)]
  gsub("[^a-z0-9]", "", tolower(ascii))
}

provider_team_aliases <- function() {
  base <- super_lig_teams() |>
    dplyr::transmute(alias = normalise_provider_name(team), team_id, team)
  extras <- tibble::tribble(
    ~alias_raw, ~team,
    "Fenerbahce", "Fenerbahçe",
    "Besiktas", "Beşiktaş",
    "Istanbul Basaksehir", "İstanbul Başakşehir",
    "Istanbul Basaksehir FK", "İstanbul Başakşehir",
    "Goztepe", "Göztepe",
    "Goztep", "Göztepe",
    "Corum FK", "Çorum FK",
    "Corum", "Çorum FK",
    "Corum Belediyespor", "Çorum FK",
    "Rizespor", "Çaykur Rizespor",
    "Caykur Rizespor", "Çaykur Rizespor",
    "Kasimpasa", "Kasımpaşa",
    "Gazisehir Gaziantep", "Gaziantep FK",
    "Gaziantep", "Gaziantep FK",
    "Buyuksehyr", "İstanbul Başakşehir",
    "Amedspor", "Amed SK",
    "Amed Sportif Faaliyetler", "Amed SK",
    "Amed Sportif", "Amed SK",
    "Erzurum BB", "Erzurumspor",
    "Erzurumspor FK", "Erzurumspor",
    "Genclerbirligi", "Gençlerbirliği",
    "Eyupspor", "Eyüpspor"
  ) |>
    dplyr::left_join(super_lig_teams() |> dplyr::select(team_id, team), by = "team") |>
    dplyr::transmute(alias = normalise_provider_name(alias_raw), team_id, team)
  dplyr::bind_rows(base, extras) |> dplyr::distinct(alias, .keep_all = TRUE)
}

provider_team_lookup <- function(provider_name) {
  aliases <- provider_team_aliases()
  index <- match(normalise_provider_name(provider_name), aliases$alias)
  if (is.na(index)) return(list(team_id = NA_integer_, team = NA_character_))
  list(team_id = aliases$team_id[[index]], team = aliases$team[[index]])
}

parse_provider_round <- function(value) {
  value <- provider_chr(value, "")
  found <- regmatches(value, regexpr("[0-9]+$", value))
  if (!length(found) || !nzchar(found)) NA_integer_ else as.integer(found)
}

parse_provider_fixture_rows <- function(raw, config, fetched_at = Sys.time()) {
  if (length(raw) == 0) return(tibble::tibble())
  catalog <- super_lig_fixtures()
  rows <- purrr::map_dfr(raw, function(x) {
    if (provider_int(x$league$id) != config$league_id || provider_int(x$league$season) != config$season) return(NULL)
    home <- provider_team_lookup(provider_chr(x$teams$home$name))
    away <- provider_team_lookup(provider_chr(x$teams$away$name))
    round <- parse_provider_round(x$league$round)
    possible <- catalog |> dplyr::filter(home_team_id == home$team_id, away_team_id == away$team_id)
    if (!is.na(round) && nrow(possible) > 1) possible <- possible |> dplyr::filter(.data$round == !!round)
    internal_id <- if (nrow(possible) == 1) possible$fixture_id[[1]] else NA_character_
    timestamp <- provider_num(x$fixture$timestamp)
    kickoff <- if (is.na(timestamp)) as.POSIXct(provider_chr(x$fixture$date), tz = config$timezone) else as.POSIXct(timestamp, origin = "1970-01-01", tz = config$timezone)
    tibble::tibble(
      provider_fixture_id = provider_chr(x$fixture$id),
      internal_fixture_id = internal_id,
      round = round,
      kickoff = format(kickoff, "%Y-%m-%dT%H:%M:%S%z", tz = config$timezone),
      status_short = provider_chr(x$fixture$status$short),
      status_long = provider_chr(x$fixture$status$long),
      venue = provider_chr(x$fixture$venue$name),
      home_team_provider = provider_chr(x$teams$home$name),
      away_team_provider = provider_chr(x$teams$away$name),
      home_goals = provider_int(x$goals$home),
      away_goals = provider_int(x$goals$away),
      last_synced_at = format(fetched_at, "%Y-%m-%dT%H:%M:%S%z")
    )
  })
  rows |> dplyr::filter(!is.na(provider_fixture_id), nzchar(provider_fixture_id))
}

provider_position <- function(value) {
  dplyr::recode(toupper(provider_chr(value, "")), G = "GK", D = "DEF", M = "MID", F = "FWD", .default = "MID")
}

parse_provider_lineups <- function(raw, provider_fixture_id, fetched_at = Sys.time()) {
  fetched <- format(fetched_at, "%Y-%m-%dT%H:%M:%S%z")
  if (length(raw) == 0) return(tibble::tibble())
  purrr::map_dfr(raw, function(team_lineup) {
    team <- provider_team_lookup(provider_chr(team_lineup$team$name))
    if (is.na(team$team_id)) return(NULL)
    parse_group <- function(group, is_starting) {
      if (is.null(group) || length(group) == 0) return(tibble::tibble())
      purrr::map_dfr(group, function(item) {
        player_info <- item$player %||% item
        player_name <- provider_chr(player_info$name, "Bilinmeyen oyuncu")
        player_id <- provider_chr(player_info$id, paste0("name-", normalise_provider_name(player_name)))
        tibble::tibble(
          provider_fixture_id = as.character(provider_fixture_id),
          team_id = as.integer(team$team_id),
          player_id = player_id,
          player = player_name,
          position = provider_position(player_info$pos),
          shirt_number = provider_int(player_info$number),
          grid = provider_chr(player_info$grid),
          is_starting = as.integer(is_starting),
          formation = provider_chr(team_lineup$formation),
          coach = provider_chr(team_lineup$coach$name),
          fetched_at = fetched
        )
      })
    }
    dplyr::bind_rows(parse_group(team_lineup$startXI, TRUE), parse_group(team_lineup$substitutes, FALSE))
  }) |>
    dplyr::distinct(provider_fixture_id, team_id, player_id, is_starting, .keep_all = TRUE)
}

parse_provider_absences <- function(raw, provider_fixture_id, fetched_at = Sys.time()) {
  fetched <- format(fetched_at, "%Y-%m-%dT%H:%M:%S%z")
  if (length(raw) == 0) return(tibble::tibble())
  purrr::map_dfr(raw, function(item) {
    team <- provider_team_lookup(provider_chr(item$team$name))
    if (is.na(team$team_id)) return(NULL)
    player_name <- provider_chr(item$player$name, "Bilinmeyen oyuncu")
    tibble::tibble(
      provider_fixture_id = as.character(provider_fixture_id),
      team_id = as.integer(team$team_id),
      player_id = provider_chr(item$player$id, paste0("name-", normalise_provider_name(player_name))),
      player = player_name,
      absence_type = provider_chr(item$player$type),
      reason = provider_chr(item$player$reason),
      fetched_at = fetched
    )
  }) |>
    dplyr::distinct(provider_fixture_id, team_id, player_id, .keep_all = TRUE)
}

parse_api_football_odds <- function(raw, fixture_map, fetched_at = Sys.time(), source_url = "https://v3.football.api-sports.io/odds") {
  if (length(raw) == 0 || nrow(fixture_map) == 0) return(tibble::tibble())
  lookup <- fixture_map |>
    dplyr::filter(!is.na(internal_fixture_id), nzchar(internal_fixture_id)) |>
    dplyr::distinct(provider_fixture_id, internal_fixture_id)
  fetched <- format(fetched_at, "%Y-%m-%dT%H:%M:%S%z")

  rows <- purrr::map_dfr(raw, function(event) {
    provider_id <- provider_chr(event$fixture$id)
    internal_id <- lookup$internal_fixture_id[match(provider_id, lookup$provider_fixture_id)]
    if (length(internal_id) == 0 || is.na(internal_id)) return(NULL)
    snapshot <- provider_chr(event$update, fetched)
    bookmakers <- event$bookmakers %||% list()
    purrr::map_dfr(bookmakers, function(bookmaker) {
      bookmaker_name <- provider_chr(bookmaker$name, "Bilinmeyen bahis şirketi")
      purrr::map_dfr(bookmaker$bets %||% list(), function(bet) {
        bet_key <- normalise_provider_name(provider_chr(bet$name))
        purrr::map_dfr(bet$values %||% list(), function(value) {
          label <- provider_chr(value$value, "")
          label_key <- normalise_provider_name(label)
          market <- selection <- NA_character_
          if (bet_key %in% c("matchwinner", "1x2", "winner")) {
            market <- "result"
            selection <- dplyr::case_when(
              label_key %in% c("home", "1") ~ "home",
              label_key %in% c("draw", "x") ~ "draw",
              label_key %in% c("away", "2") ~ "away",
              TRUE ~ NA_character_
            )
          } else if (bet_key %in% c("goalsoverunder", "overunder", "totalgoals")) {
            total <- stringr::str_extract(label, "[0-9]+(?:[\\.,][0-9]+)?")
            if (!is.na(total) && identical(gsub(",", ".", total), "2.5")) {
              market <- "ou_2_5"
              selection <- dplyr::case_when(
                grepl("^over", label_key) ~ "over",
                grepl("^under", label_key) ~ "under",
                TRUE ~ NA_character_
              )
            }
          }
          odd <- provider_num(value$odd)
          if (is.na(market) || is.na(selection) || !is.finite(odd) || odd <= 1) return(NULL)
          tibble::tibble(
            source = API_FOOTBALL_SOURCE,
            source_url = source_url,
            fixture_id = internal_id,
            market_id = market,
            selection_id = selection,
            bookmaker = bookmaker_name,
            odds = odd,
            snapshot_kind = "upcoming",
            snapshot_at = snapshot,
            fetched_at = fetched
          )
        })
      })
    })
  })
  rows |> dplyr::distinct(source, fixture_id, market_id, selection_id, bookmaker, snapshot_kind, snapshot_at, .keep_all = TRUE)
}

write_provider_schedule_cache <- function(rows, config) {
  mapped <- rows |>
    dplyr::filter(!is.na(internal_fixture_id), nzchar(internal_fixture_id)) |>
    dplyr::select(internal_fixture_id, provider_fixture_id, kickoff, venue, status_short, last_synced_at)
  dir.create(config$cache_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(mapped, file.path(config$cache_dir, "provider_fixtures.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(mapped)
}

fetch_upcoming_fixtures <- function(config, next_n = 15L) {
  raw <- api_football_get(config, "/fixtures", list(league = config$league_id, season = config$season, `next` = next_n, timezone = config$timezone))
  parse_provider_fixture_rows(raw, config) |>
    dplyr::transmute(
      fixture_id = provider_fixture_id,
      kickoff = parse_model_timestamp(kickoff),
      home_team = home_team_provider,
      away_team = away_team_provider,
      league = SUPER_LIG_COMPETITION,
      status = status_short
    )
}

fetch_fixture_snapshot <- function(config, fixture_id) {
  fixture <- api_football_get(config, "/fixtures", list(id = fixture_id, timezone = config$timezone))
  if (length(fixture) != 1L || provider_int(fixture[[1]]$league$id) != config$league_id || provider_int(fixture[[1]]$league$season) != config$season) {
    stop("Bu fixture 2026-27 Türkiye Süper Ligi'ne ait değil; uygulama başka ligleri analiz etmez.")
  }
  lineups <- api_football_get(config, "/fixtures/lineups", list(fixture = fixture_id))
  injuries <- api_football_get(config, "/injuries", list(fixture = fixture_id, timezone = config$timezone))
  list(fixture = fixture, lineups = lineups, injuries = injuries)
}

stored_payload_is_stale <- function(provider_fixture_id, kind, db_path, max_age_hours, now = Sys.time()) {
  fetched <- provider_payload_fetched_at(provider_fixture_id, kind, db_path)
  is.na(fetched) || as.numeric(difftime(now, fetched, units = "hours")) >= max_age_hours
}

automatic_result_rows <- function(fixtures) {
  ready <- fixtures |>
    dplyr::filter(status_short %in% c("FT", "AET", "PEN"), !is.na(home_goals), !is.na(away_goals), !is.na(internal_fixture_id))
  if (nrow(ready) == 0) return(tibble::tibble())
  teams <- super_lig_teams() |> dplyr::select(team_id, team)
  catalog <- super_lig_fixtures() |>
    dplyr::left_join(teams |> dplyr::rename(home_team_id = team_id, home_team = team), by = "home_team_id") |>
    dplyr::left_join(teams |> dplyr::rename(away_team_id = team_id, away_team = team), by = "away_team_id")
  ready |>
    dplyr::left_join(catalog |> dplyr::select(internal_fixture_id = fixture_id, home_team, away_team), by = "internal_fixture_id") |>
    dplyr::transmute(fixture_id = internal_fixture_id, match_date = kickoff, home_team, away_team, home_goals, away_goals)
}

provider_stat_value <- function(raw, provider_team_name, stat_names) {
  if (length(raw) == 0) return(NA_real_)
  team_key <- normalise_provider_name(provider_team_name)
  entry <- purrr::keep(raw, ~ identical(normalise_provider_name(provider_chr(.x$team$name)), team_key))
  if (length(entry) == 0) return(NA_real_)
  stats <- entry[[1]]$statistics %||% list()
  wanted <- normalise_provider_name(stat_names)
  found <- purrr::keep(stats, ~ normalise_provider_name(provider_chr(.x$type)) %in% wanted)
  if (length(found) == 0) return(NA_real_)
  value <- found[[1]]$value
  if (is.character(value)) value <- sub("%$", "", value)
  provider_num(value)
}

provider_card_count <- function(events, provider_team_name) {
  if (length(events) == 0) return(NA_integer_)
  key <- normalise_provider_name(provider_team_name)
  cards <- purrr::keep(events, ~ identical(provider_chr(.x$type, ""), "Card") && identical(normalise_provider_name(provider_chr(.x$team$name)), key))
  as.integer(length(cards))
}

enrich_automatic_result <- function(fixture_row, statistics, events) {
  automatic_result_rows(fixture_row) |>
    dplyr::mutate(
      home_xg = provider_stat_value(statistics, fixture_row$home_team_provider[[1]], c("expected_goals", "Expected Goals")),
      away_xg = provider_stat_value(statistics, fixture_row$away_team_provider[[1]], c("expected_goals", "Expected Goals")),
      home_cards = provider_card_count(events, fixture_row$home_team_provider[[1]]),
      away_cards = provider_card_count(events, fixture_row$away_team_provider[[1]])
    )
}

apply_provider_context <- function(data, db_path) {
  context <- provider_fixture_context(data$fixture$fixture_id[[1]], db_path)
  if (nrow(context$fixture) == 0) {
    data$availability <- tibble::tibble()
    data$provider_status <- NULL
    return(data)
  }
  data$provider_status <- context$fixture[1, ]
  data$availability <- context$absences |> dplyr::left_join(data$teams |> dplyr::select(team_id, team), by = "team_id")
  if (nrow(context$absences) > 0 && "player" %in% names(data$players)) {
    absent_keys <- normalise_provider_name(context$absences$player)
    matched <- normalise_provider_name(data$players$player) %in% absent_keys
    data$players$fitness[matched] <- pmin(data$players$fitness[matched], 15)
    data$players$start_score[matched] <- pmin(data$players$start_score[matched], .05)
    affected <- unique(data$players$team_id[matched])
    for (team_id in affected) {
      if (!any(data$players$team_id == team_id & data$players$identity_status == "role_prior")) {
        data$players <- dplyr::bind_rows(data$players, super_lig_role_players(data$teams |> dplyr::filter(.data$team_id == !!team_id)))
      }
    }
  }
  starters <- context$lineups |> dplyr::filter(is_starting == 1L)
  for (team_id in unique(starters$team_id)) {
    official <- starters |> dplyr::filter(.data$team_id == !!team_id)
    if (nrow(official) != 11L) next
    defaults <- tibble::tibble(
      position = c("GK", "DEF", "MID", "FWD"), role = c("Kaleci", "Savunmacı", "Orta saha", "Hücumcu"),
      goals_p90 = c(0.00, 0.05, 0.15, 0.35), cards_p90 = c(0.04, 0.28, 0.25, 0.18)
    )
    official_players <- official |>
      dplyr::left_join(defaults, by = "position") |>
      dplyr::transmute(
        player_id = suppressWarnings(as.integer(player_id)), team_id = as.integer(team_id), player, position, role,
        start_score = 1, minutes_share = .95, goals_p90, cards_p90, form = 72, fitness = 100,
        identity_status = "official_lineup"
      ) |>
      dplyr::mutate(player_id = dplyr::if_else(is.na(player_id), as.integer(team_id * 100000L + dplyr::row_number()), player_id))
    data$players <- data$players |> dplyr::filter(.data$team_id != !!team_id) |> dplyr::bind_rows(official_players)
    formation_value <- official$formation[[1]]
    coach_value <- official$coach[[1]]
    data$teams <- data$teams |>
      dplyr::mutate(
        formation = dplyr::if_else(.data$team_id == !!team_id & !is.na(formation_value), formation_value, formation),
        coach = dplyr::if_else(.data$team_id == !!team_id & !is.na(coach_value), coach_value, coach),
        lineup_status = dplyr::if_else(.data$team_id == !!team_id, "API resmî ilk 11", lineup_status)
      )
  }
  data
}

parse_provider_coverage <- function(raw, season) {
  empty <- list(injuries = FALSE, lineups = FALSE, odds = FALSE, events = FALSE, statistics = FALSE, players = FALSE)
  if (length(raw) == 0) return(empty)
  seasons <- raw[[1]]$seasons %||% list()
  selected <- purrr::keep(seasons, ~ provider_int(.x$year) == as.integer(season))
  if (length(selected) == 0) return(empty)
  coverage <- selected[[1]]$coverage %||% list()
  fixtures <- coverage$fixtures %||% list()
  list(
    injuries = isTRUE(coverage$injuries),
    lineups = isTRUE(fixtures$lineups),
    odds = isTRUE(coverage$odds),
    events = isTRUE(fixtures$events),
    statistics = isTRUE(fixtures$statistics_fixtures),
    players = isTRUE(fixtures$statistics_players)
  )
}

auto_sync_league <- function(config, now = Sys.time(), detail_budget = config$sync_detail_budget %||% 8L) {
  if (!api_football_enabled(config)) stop("Ücretsiz API anahtarı ayarlı değil; senkronizasyon başlatılmadı.")
  initialize_store(config$db_path)
  started_at <- Sys.time()
  requests_used <- 0L
  seen <- 0L
  mapped_n <- 0L
  odds_count <- 0L
  used_last_24h <- sync_requests_last_24h(config$db_path, now)
  if (used_last_24h >= 90L) {
    message <- "Ücretsiz günlük kota için güvenlik payı doldu; yeni çağrı yapılmadı."
    record_sync_run(config$db_path, started_at, "ok", 0L, 0L, 0L, message)
    return(invisible(list(status = "quota_guard", requests_used = 0L, fixtures_seen = 0L, fixtures_mapped = 0L, results = 0L)))
  }
  detail_budget <- min(as.integer(detail_budget), max(0L, 89L - used_last_24h))
  get_data <- function(endpoint, query) {
    requests_used <<- requests_used + 1L
    api_football_get(config, endpoint, query)
  }
  tryCatch({
    raw_fixtures <- get_data("/fixtures", list(league = config$league_id, season = config$season, timezone = config$timezone))
    fixtures <- parse_provider_fixture_rows(raw_fixtures, config, now)
    seen <- nrow(fixtures)
    mapped <- fixtures |> dplyr::filter(!is.na(internal_fixture_id), nzchar(internal_fixture_id))
    mapped_n <- nrow(mapped)
    if (seen > 0 && mapped_n / seen < .90) stop("Sağlayıcı fikstürünün %90'dan azı iç fikstürle eşleşti; veri güvenliği için kayıt durduruldu.")
    store_provider_fixtures(mapped, config$db_path)
    write_provider_schedule_cache(mapped, config)
    results <- automatic_result_rows(mapped)
    if (nrow(results) > 0) import_postmatch_results(results, config$db_path)

    remaining <- max(0L, as.integer(detail_budget))
    coverage_key <- paste0("league-", config$league_id, "-", config$season)
    if (remaining > 0 && stored_payload_is_stale(coverage_key, "coverage", config$db_path, 24, now)) {
      coverage_raw <- get_data("/leagues", list(id = config$league_id, season = config$season))
      store_provider_payload(coverage_key, "coverage", coverage_raw, config$db_path, now)
      remaining <- remaining - 1L
    } else {
      coverage_raw <- read_provider_payload(coverage_key, "coverage", config$db_path) %||% list()
    }
    coverage <- parse_provider_coverage(coverage_raw, config$season)
    kickoff_time <- parse_model_timestamp(mapped$kickoff)
    upcoming <- mapped |>
      dplyr::mutate(kickoff_time = kickoff_time, hours_to_kickoff = as.numeric(difftime(kickoff_time, now, units = "hours"))) |>
      dplyr::filter(hours_to_kickoff >= -3, hours_to_kickoff <= 48)
    # Resmî ilk 11 en zaman-kritik veri olduğundan önce alınır.
    if (nrow(upcoming) > 0) for (i in seq_len(nrow(upcoming))) {
      if (remaining <= 0) break
      fixture <- upcoming[i, ]
      provider_id <- fixture$provider_fixture_id[[1]]
      if (coverage$lineups && fixture$hours_to_kickoff[[1]] <= 1.5 && stored_payload_is_stale(provider_id, "lineups", config$db_path, .4, now)) {
        raw <- get_data("/fixtures/lineups", list(fixture = provider_id))
        replace_provider_snapshot("provider_lineups", provider_id, parse_provider_lineups(raw, provider_id, now), config$db_path)
        store_provider_payload(provider_id, "lineups", raw, config$db_path, now)
        remaining <- remaining - 1L
      }
    }

    # Bir tarih çağrısı o günkü tüm lig maçlarını kapsar; ücretsiz kotayı fixture başına harcamaz.
    if (coverage$odds && nrow(upcoming) > 0 && remaining > 0) {
      odds_dates <- unique(format(upcoming$kickoff_time, "%Y-%m-%d", tz = config$timezone))
      for (match_date in odds_dates) {
        if (remaining <= 0) break
        payload_key <- paste0("league-", config$league_id, "-odds-", match_date)
        if (!stored_payload_is_stale(payload_key, "odds", config$db_path, 3, now)) next
        raw <- get_data("/odds", list(league = config$league_id, season = config$season, date = match_date, page = 1L))
        parsed_odds <- parse_api_football_odds(raw, mapped, now)
        if (nrow(parsed_odds) > 0) odds_count <- odds_count + store_odds_snapshots(parsed_odds, config$db_path)
        store_provider_payload(payload_key, "odds", raw, config$db_path, now)
        remaining <- remaining - 1L
      }
    }

    if (nrow(upcoming) > 0) for (i in seq_len(nrow(upcoming))) {
      if (remaining <= 0) break
      fixture <- upcoming[i, ]
      provider_id <- fixture$provider_fixture_id[[1]]
      if (coverage$injuries && fixture$hours_to_kickoff[[1]] <= 36 && stored_payload_is_stale(provider_id, "injuries", config$db_path, 4, now)) {
        raw <- get_data("/injuries", list(fixture = provider_id, timezone = config$timezone))
        replace_provider_snapshot("provider_absences", provider_id, parse_provider_absences(raw, provider_id, now), config$db_path)
        store_provider_payload(provider_id, "injuries", raw, config$db_path, now)
        remaining <- remaining - 1L
      }
    }

    completed <- mapped |> dplyr::filter(status_short %in% c("FT", "AET", "PEN")) |> dplyr::arrange(dplyr::desc(parse_model_timestamp(kickoff)))
    if (nrow(completed) > 0) for (i in seq_len(nrow(completed))) {
      if (remaining <= 0) break
      fixture <- completed[i, ]
      provider_id <- fixture$provider_fixture_id[[1]]
      kinds <- c(
        if (coverage$statistics) c(statistics = "/fixtures/statistics"),
        if (coverage$events) c(events = "/fixtures/events"),
        if (coverage$players) c(players = "/fixtures/players")
      )
      fetched <- list()
      if (length(kinds) > 0) for (kind in names(kinds)) {
        if (remaining <= 0) break
        if (stored_payload_is_stale(provider_id, kind, config$db_path, 24 * 365, now)) {
          raw <- get_data(kinds[[kind]], list(fixture = provider_id))
          store_provider_payload(provider_id, kind, raw, config$db_path, now)
          fetched[[kind]] <- raw
          remaining <- remaining - 1L
        }
      }
      if (!is.null(fetched$statistics) || !is.null(fetched$events)) {
        import_postmatch_results(enrich_automatic_result(fixture, fetched$statistics %||% list(), fetched$events %||% list()), config$db_path)
      }
    }

    if (nrow(upcoming) > 0) for (i in seq_len(nrow(upcoming))) {
      fixture <- upcoming[i, ]
      if (fixture$hours_to_kickoff[[1]] <= 0 || fixture$hours_to_kickoff[[1]] > 36) next
      internal_id <- fixture$internal_fixture_id[[1]]
      if (!analysis_exists_for_fixture(internal_id, config$db_path)) {
        data <- super_lig_match_data(internal_id) |> apply_provider_context(config$db_path) |> apply_postmatch_learning(config$db_path)
        record_analysis(build_prediction(data), config$db_path)
      }
    }
    message <- paste(mapped_n, "fikstür eşleşti;", nrow(results), "tamamlanmış sonuç ve", odds_count, "oran satırı işlendi.")
    record_sync_run(config$db_path, started_at, "ok", requests_used, seen, mapped_n, message)
    invisible(list(status = "ok", requests_used = requests_used, fixtures_seen = seen, fixtures_mapped = mapped_n, results = nrow(results), odds_rows = odds_count, message = message))
  }, error = function(e) {
    record_sync_run(config$db_path, started_at, "error", requests_used, seen, mapped_n, conditionMessage(e))
    stop(e)
  })
}
