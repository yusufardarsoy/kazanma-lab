FOOTBALL_DATA_SOURCE <- "Football-Data.co.uk"

football_data_cache_path <- function(config, kind = c("season", "fixtures")) {
  kind <- match.arg(kind)
  file.path(
    config$cache_dir,
    if (kind == "season") "football_data_t1_2627.csv" else "football_data_fixtures.csv"
  )
}

validate_football_data_text <- function(text) {
  text <- sub("^\ufeff", "", text)
  if (!grepl("^Div,Date,Time,HomeTeam,AwayTeam", text)) {
    stop("Football-Data CSV başlığı doğrulanamadı; önbellek güncellenmedi.")
  }
  text
}

fetch_football_data_csv <- function(url, cache_path, now = Sys.time()) {
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  error_message <- ""
  downloaded <- tryCatch({
    response <- httr2::request(url) |>
      httr2::req_user_agent("Kazanma-Lab/0.4 personal analytics") |>
      httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()
    text <- validate_football_data_text(httr2::resp_body_string(response))
    last_modified <- httr2::resp_header(response, "last-modified")
    published_at <- suppressWarnings(as.POSIXct(
      last_modified, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"
    ))
    if (length(published_at) != 1L || is.na(published_at)) published_at <- now
    writeLines(text, cache_path, useBytes = TRUE)
    suppressWarnings(try(Sys.setFileTime(cache_path, published_at), silent = TRUE))
    list(text = text, fetched_at = published_at, from_network = TRUE, error = "")
  }, error = function(e) {
    error_message <<- conditionMessage(e)
    NULL
  })
  if (!is.null(downloaded)) return(downloaded)
  if (!file.exists(cache_path)) {
    stop("Football-Data indirilemedi ve yerel önbellek yok: ", error_message)
  }
  cached <- paste(readLines(cache_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  cached <- validate_football_data_text(cached)
  list(
    text = cached,
    fetched_at = file.info(cache_path)$mtime[[1]],
    from_network = FALSE,
    error = error_message
  )
}

read_football_data_csv_text <- function(text) {
  text <- validate_football_data_text(text)
  connection <- textConnection(text, encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  utils::read.csv(
    connection, check.names = FALSE, stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  ) |>
    tibble::as_tibble()
}

football_data_num <- function(data, name) {
  if (!name %in% names(data)) return(rep(NA_real_, nrow(data)))
  suppressWarnings(as.numeric(data[[name]]))
}

football_data_catalog <- function() {
  utils::read.csv(
    file.path(SUPER_LIG_PROJECT_ROOT, "data", "super_lig_fixture_catalog.csv"),
    check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  ) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      fixture_id = as.character(fixture_id), round = as.integer(round),
      home_team = as.character(home_team), away_team = as.character(away_team)
    )
}

parse_football_data_rows <- function(data, fetched_at = Sys.time(), source_url = "") {
  data <- data |>
    dplyr::filter(.data$Div == "T1")
  if (nrow(data) == 0) return(tibble::tibble())

  home_lookup <- lapply(data$HomeTeam, provider_team_lookup)
  away_lookup <- lapply(data$AwayTeam, provider_team_lookup)
  home_team <- vapply(home_lookup, function(x) x$team, character(1))
  away_team <- vapply(away_lookup, function(x) x$team, character(1))
  kickoff_london <- as.POSIXct(
    paste(data$Date, data$Time), format = "%d/%m/%Y %H:%M", tz = "Europe/London"
  )
  kickoff <- format(kickoff_london, "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Istanbul")
  completed <- !is.na(football_data_num(data, "FTHG")) & !is.na(football_data_num(data, "FTAG"))
  fetched_at_text <- format(fetched_at, "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Istanbul")

  mapped <- tibble::tibble(
    source_row = seq_len(nrow(data)),
    home_team_provider = as.character(data$HomeTeam),
    away_team_provider = as.character(data$AwayTeam),
    home_team = home_team,
    away_team = away_team,
    kickoff = kickoff
  ) |>
    dplyr::left_join(football_data_catalog(), by = c("home_team", "away_team"))

  data |>
    dplyr::mutate(source_row = dplyr::row_number(), completed = completed) |>
    dplyr::left_join(mapped, by = "source_row") |>
    dplyr::mutate(
      source = FOOTBALL_DATA_SOURCE,
      source_url = .env$source_url,
      fetched_at = .env$fetched_at_text
    )
}

validate_football_data_rows <- function(rows, label) {
  if (nrow(rows) == 0) stop(label, " dosyasında T1 satırı bulunamadı.")
  mapped <- sum(!is.na(rows$fixture_id) & nzchar(rows$fixture_id))
  if (mapped / nrow(rows) < .90) {
    unknown <- unique(c(rows$home_team_provider[is.na(rows$fixture_id)], rows$away_team_provider[is.na(rows$fixture_id)]))
    stop(label, " eşleşme oranı %90'ın altında. Bilinmeyen adlar: ", paste(unknown, collapse = ", "))
  }
  if (anyNA(rows$kickoff) || any(!nzchar(rows$kickoff))) stop(label, " içinde geçersiz maç tarihi var.")
  if (anyDuplicated(rows$fixture_id[!is.na(rows$fixture_id)])) stop(label, " aynı Süper Lig maçını birden çok kez içeriyor.")
  invisible(TRUE)
}

football_data_schedule_rows <- function(rows) {
  rows |>
    dplyr::filter(!is.na(fixture_id), nzchar(fixture_id)) |>
    dplyr::transmute(
      internal_fixture_id = as.character(fixture_id),
      kickoff = as.character(kickoff),
      status_short = dplyr::if_else(completed, "FT", "NS"),
      source = source,
      source_url = source_url,
      last_synced_at = fetched_at
    )
}

write_public_schedule_cache <- function(rows, config) {
  path <- file.path(config$cache_dir, "public_fixtures.csv")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  clean <- rows |>
    dplyr::arrange(internal_fixture_id, dplyr::desc(status_short == "FT"), dplyr::desc(last_synced_at)) |>
    dplyr::distinct(internal_fixture_id, .keep_all = TRUE)
  utils::write.csv(clean, path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  invisible(path)
}

football_data_result_rows <- function(rows) {
  if (nrow(rows) == 0) return(tibble::tibble())
  home_xg <- football_data_num(rows, "HxG")
  away_xg <- football_data_num(rows, "AxG")
  home_cards <- football_data_num(rows, "HY") + football_data_num(rows, "HR")
  away_cards <- football_data_num(rows, "AY") + football_data_num(rows, "AR")
  tibble::tibble(
    fixture_id = as.character(rows$fixture_id),
    match_date = as.character(rows$kickoff),
    home_team = as.character(rows$home_team),
    away_team = as.character(rows$away_team),
    home_goals = as.integer(football_data_num(rows, "FTHG")),
    away_goals = as.integer(football_data_num(rows, "FTAG")),
    home_xg = home_xg,
    away_xg = away_xg,
    home_cards = as.integer(home_cards),
    away_cards = as.integer(away_cards),
    result_source = FOOTBALL_DATA_SOURCE,
    source_url = as.character(rows$source_url),
    source_published_at = as.character(rows$fetched_at),
    fetched_at = as.character(rows$fetched_at)
  ) |>
    dplyr::filter(!is.na(fixture_id), !is.na(home_goals), !is.na(away_goals))
}

football_data_odds_rows <- function(rows, snapshot_kind = "upcoming") {
  if (nrow(rows) == 0) return(tibble::tibble())
  closing <- identical(snapshot_kind, "closing")
  h_names <- if (closing) c("AvgCH", "AvgH", "B365CH", "B365H") else c("AvgH", "B365H")
  d_names <- if (closing) c("AvgCD", "AvgD", "B365CD", "B365D") else c("AvgD", "B365D")
  a_names <- if (closing) c("AvgCA", "AvgA", "B365CA", "B365A") else c("AvgA", "B365A")
  over_names <- if (closing) c("AvgC>2.5", "Avg>2.5", "B365C>2.5", "B365>2.5") else c("Avg>2.5", "B365>2.5")
  under_names <- if (closing) c("AvgC<2.5", "Avg<2.5", "B365C<2.5", "B365<2.5") else c("Avg<2.5", "B365<2.5")
  first_available <- function(names) {
    values <- rep(NA_real_, nrow(rows))
    for (name in names) {
      candidate <- football_data_num(rows, name)
      values[is.na(values)] <- candidate[is.na(values)]
    }
    values
  }
  values <- list(
    home = first_available(h_names), draw = first_available(d_names), away = first_available(a_names),
    over = first_available(over_names), under = first_available(under_names)
  )
  selections <- tibble::tribble(
    ~market_id, ~selection_id, ~value_name,
    "result", "home", "home",
    "result", "draw", "draw",
    "result", "away", "away",
    "ou_2_5", "over", "over",
    "ou_2_5", "under", "under"
  )
  purrr::map_dfr(seq_len(nrow(rows)), function(i) {
    selections |>
      dplyr::mutate(
        source = FOOTBALL_DATA_SOURCE,
        source_url = as.character(rows$source_url[[i]]),
        fixture_id = as.character(rows$fixture_id[[i]]),
        bookmaker = "Piyasa ortalaması",
        odds = vapply(value_name, function(name) values[[name]][[i]], numeric(1)),
        snapshot_kind = snapshot_kind,
        snapshot_at = as.character(rows$fetched_at[[i]]),
        fetched_at = as.character(rows$fetched_at[[i]])
      ) |>
      dplyr::select(source, source_url, fixture_id, market_id, selection_id, bookmaker, odds, snapshot_kind, snapshot_at, fetched_at)
  }) |>
    dplyr::filter(!is.na(fixture_id), is.finite(odds), odds > 1)
}

freeze_public_upcoming_predictions <- function(schedule, config, now = Sys.time()) {
  if (nrow(schedule) == 0) return(0L)
  kickoff_time <- parse_model_timestamp(schedule$kickoff)
  candidates <- schedule |>
    dplyr::mutate(
      kickoff_time = kickoff_time,
      hours_to_kickoff = as.numeric(difftime(kickoff_time, now, units = "hours"))
    ) |>
    dplyr::filter(status_short != "FT", hours_to_kickoff > 0, hours_to_kickoff <= 36)
  frozen <- 0L
  if (nrow(candidates) == 0) return(frozen)
  for (fixture_id in candidates$internal_fixture_id) {
    if (analysis_exists_for_fixture(fixture_id, config$db_path)) next
    data <- super_lig_match_data(fixture_id)
    if (exists("apply_provider_context", mode = "function")) data <- apply_provider_context(data, config$db_path)
    data <- apply_postmatch_learning(data, config$db_path)
    record_analysis(build_prediction(data), config$db_path)
    frozen <- frozen + 1L
  }
  frozen
}

football_data_sync_is_fresh <- function(config, now = Sys.time()) {
  latest <- latest_public_sync_run(config$db_path, FOOTBALL_DATA_SOURCE)
  if (nrow(latest) == 0 || latest$status[[1]] != "ok") return(FALSE)
  last_time <- parse_model_timestamp(latest$started_at)[[1]]
  !is.na(last_time) && as.numeric(difftime(now, last_time, units = "hours")) < config$public_data_refresh_hours
}

sync_public_football_data <- function(config, now = Sys.time(), force = FALSE) {
  initialize_store(config$db_path)
  if (!force && football_data_sync_is_fresh(config, now)) {
    return(invisible(list(
      source = FOOTBALL_DATA_SOURCE, status = "fresh", fixtures_seen = 0L,
      fixtures_mapped = 0L, results = 0L, odds_rows = 0L, frozen = 0L,
      message = "6 saatlik kaynak yenileme freni etkin."
    )))
  }
  started_at <- Sys.time()
  seen <- mapped <- result_count <- odds_count <- 0L
  tryCatch({
    season_fetch <- fetch_football_data_csv(
      config$football_data_season_url, football_data_cache_path(config, "season"), now
    )
    fixture_fetch <- fetch_football_data_csv(
      config$football_data_fixtures_url, football_data_cache_path(config, "fixtures"), now
    )
    season <- parse_football_data_rows(
      read_football_data_csv_text(season_fetch$text), season_fetch$fetched_at,
      config$football_data_season_url
    )
    fixtures <- parse_football_data_rows(
      read_football_data_csv_text(fixture_fetch$text), fixture_fetch$fetched_at,
      config$football_data_fixtures_url
    )
    validate_football_data_rows(season, "Sezon sonuç")
    validate_football_data_rows(fixtures, "Güncel fikstür")

    seen <- nrow(season) + nrow(fixtures)
    mapped <- sum(!is.na(c(season$fixture_id, fixtures$fixture_id)))
    schedule <- dplyr::bind_rows(
      football_data_schedule_rows(season), football_data_schedule_rows(fixtures)
    )
    write_public_schedule_cache(schedule, config)

    results <- dplyr::bind_rows(
      football_data_result_rows(season |> dplyr::filter(completed)),
      football_data_result_rows(fixtures |> dplyr::filter(completed))
    ) |>
      dplyr::distinct(fixture_id, .keep_all = TRUE)
    if (nrow(results) > 0) result_count <- import_postmatch_results(results, config$db_path)
    odds <- dplyr::bind_rows(
      football_data_odds_rows(season |> dplyr::filter(completed), "closing"),
      football_data_odds_rows(fixtures, "upcoming")
    )
    if (nrow(odds) > 0) odds_count <- store_odds_snapshots(odds, config$db_path)
    frozen <- freeze_public_upcoming_predictions(schedule, config, now)
    network_ok <- isTRUE(season_fetch$from_network) && isTRUE(fixture_fetch$from_network)
    status <- if (network_ok) "ok" else "cached"
    cache_note <- if (network_ok) "resmî CSV indirildi" else "ağ erişimi yok; doğrulanmış yerel önbellek kullanıldı"
    message <- paste0(
      mapped, "/", seen, " fikstür eşleşti; ", result_count, " sonuç, ", odds_count,
      " oran satırı işlendi; ", frozen, " yaklaşan tahmin donduruldu; ", cache_note, "."
    )
    record_public_sync_run(
      config$db_path, FOOTBALL_DATA_SOURCE, started_at, status,
      requests_used = 2L, fixtures_seen = seen, fixtures_mapped = mapped,
      results_imported = result_count, odds_rows = odds_count, message = message
    )
    invisible(list(
      source = FOOTBALL_DATA_SOURCE, status = status, fixtures_seen = seen,
      fixtures_mapped = mapped, results = result_count, odds_rows = odds_count,
      frozen = frozen, message = message
    ))
  }, error = function(e) {
    record_public_sync_run(
      config$db_path, FOOTBALL_DATA_SOURCE, started_at, "error", requests_used = 2L,
      fixtures_seen = seen, fixtures_mapped = mapped, results_imported = result_count,
      odds_rows = odds_count, message = conditionMessage(e)
    )
    stop(e)
  })
}

sync_public_scoreboard_scores <- function(config, now = Sys.time()) {
  initialize_store(config$db_path)
  catalog <- football_data_catalog()
  start_d <- format(now - 60 * 86400, "%Y%m%d")
  end_d <- format(now + 7 * 86400, "%Y%m%d")
  date_query <- paste0(start_d, "-", end_d)

  raw <- tryCatch({
    httr2::request("https://site.api.espn.com/apis/site/v2/sports/soccer/tur.1/scoreboard") |>
      httr2::req_url_query(dates = date_query) |>
      httr2::req_user_agent("Kazanma-Lab/0.4 personal analytics") |>
      httr2::req_timeout(25) |>
      httr2::req_retry(max_tries = 2) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = FALSE)
  }, error = function(e) NULL)

  if (is.null(raw) || length(raw$events) == 0) {
    return(invisible(list(
      source = "Açık Canlı Skor Servisi", status = "cached", results = 0L, odds_rows = 0L,
      message = "Canlı skor servisine ulaşılamadı veya aktif maç bulunamadı."
    )))
  }

  results_list <- list()
  live_list <- list()
  for (ev in raw$events) {
    comp <- ev$competitions[[1]]
    status <- ev$status$type$name
    completed <- isTRUE(ev$status$type$completed) || identical(status, "STATUS_FULL_TIME") || identical(status, "STATUS_FINAL")
    in_progress <- !completed && (
      identical(ev$status$type$state, "in") ||
      status %in% c("STATUS_IN_PROGRESS", "STATUS_FIRST_HALF", "STATUS_SECOND_HALF", "STATUS_HALFTIME", "STATUS_EXTRA_TIME")
    )

    home_comp <- purrr::keep(comp$competitors, ~ .x$homeAway == "home")[[1]]
    away_comp <- purrr::keep(comp$competitors, ~ .x$homeAway == "away")[[1]]

    home_lookup <- provider_team_lookup(home_comp$team$name)
    away_lookup <- provider_team_lookup(away_comp$team$name)
    if (is.na(home_lookup$team) || is.na(away_lookup$team)) next

    home_score <- suppressWarnings(as.integer(home_comp$score))
    away_score <- suppressWarnings(as.integer(away_comp$score))
    if (is.na(home_score) || is.na(away_score)) next

    matched <- catalog |>
      dplyr::filter(home_team == home_lookup$team, away_team == away_lookup$team)
    fixture_id <- if (nrow(matched) > 0) matched$fixture_id[[1]] else NA_character_
    if (is.na(fixture_id)) next

    match_date <- format(as.POSIXct(ev$date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Istanbul")

    if (completed) {
      results_list[[length(results_list) + 1L]] <- tibble::tibble(
        fixture_id = fixture_id,
        match_date = match_date,
        home_team = home_lookup$team,
        away_team = away_lookup$team,
        home_goals = home_score,
        away_goals = away_score,
        home_xg = NA_real_,
        away_xg = NA_real_,
        home_cards = NA_integer_,
        away_cards = NA_integer_,
        result_source = "Açık Canlı Skor Servisi",
        source_url = "https://site.api.espn.com/apis/site/v2/sports/soccer/tur.1/scoreboard",
        source_published_at = format(now, "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Istanbul"),
        fetched_at = format(now, "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Istanbul")
      )
    } else if (in_progress) {
      display_clock <- ev$status$displayClock %||% (if (status == "STATUS_HALFTIME") "İY" else "CANLI")
      detail_txt <- ev$status$type$detail %||% paste0("CANLI ", display_clock)
      live_list[[length(live_list) + 1L]] <- tibble::tibble(
        fixture_id = fixture_id,
        home_team = home_lookup$team,
        away_team = away_lookup$team,
        home_goals = home_score,
        away_goals = away_score,
        minute = as.character(display_clock),
        status_text = as.character(detail_txt),
        is_live = 1L,
        updated_at = format(now, "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Istanbul")
      )
    }
  }

  imported_count <- 0L
  if (length(results_list) > 0) {
    results_df <- dplyr::bind_rows(results_list) |>
      dplyr::distinct(fixture_id, .keep_all = TRUE)
    imported_count <- import_postmatch_results(results_df, config$db_path)
  }

  live_count <- 0L
  if (length(live_list) > 0) {
    live_df <- dplyr::bind_rows(live_list) |>
      dplyr::distinct(fixture_id, .keep_all = TRUE)
    live_count <- save_live_scores_db(config$db_path, live_df)
  }

  invisible(list(
    source = "Açık Canlı Skor Servisi",
    status = "ok",
    results = imported_count,
    live_matches = live_count,
    odds_rows = 0L,
    message = paste0(imported_count, " tamamlanan maç ve ", live_count, " canlı maç skoru güncellendi.")
  ))
}
