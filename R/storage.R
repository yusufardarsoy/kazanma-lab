with_store <- function(db_path, fn) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fn(con)
}

initialize_store <- function(db_path) {
  with_store(db_path, function(con) {
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS analyses (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        fixture_id TEXT NOT NULL,
        home_team TEXT NOT NULL,
        away_team TEXT NOT NULL,
        home_win REAL NOT NULL,
        draw REAL NOT NULL,
        away_win REAL NOT NULL,
        expected_home_goals REAL NOT NULL,
        expected_away_goals REAL NOT NULL,
        model_version TEXT NOT NULL,
        data_mode TEXT NOT NULL,
        created_at TEXT NOT NULL
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS postmatch_results (
        fixture_id TEXT PRIMARY KEY,
        match_date TEXT NOT NULL,
        home_team TEXT NOT NULL,
        away_team TEXT NOT NULL,
        home_goals INTEGER NOT NULL,
        away_goals INTEGER NOT NULL,
        home_xg REAL,
        away_xg REAL,
        home_cards INTEGER,
        away_cards INTEGER,
        imported_at TEXT NOT NULL
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS provider_fixtures (
        provider_fixture_id TEXT PRIMARY KEY,
        internal_fixture_id TEXT NOT NULL UNIQUE,
        round INTEGER,
        kickoff TEXT NOT NULL,
        status_short TEXT,
        status_long TEXT,
        venue TEXT,
        home_team_provider TEXT NOT NULL,
        away_team_provider TEXT NOT NULL,
        home_goals INTEGER,
        away_goals INTEGER,
        last_synced_at TEXT NOT NULL
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS provider_lineups (
        provider_fixture_id TEXT NOT NULL,
        team_id INTEGER NOT NULL,
        player_id TEXT NOT NULL,
        player TEXT NOT NULL,
        position TEXT NOT NULL,
        shirt_number INTEGER,
        grid TEXT,
        is_starting INTEGER NOT NULL,
        formation TEXT,
        coach TEXT,
        fetched_at TEXT NOT NULL,
        PRIMARY KEY(provider_fixture_id, team_id, player_id, is_starting)
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS provider_absences (
        provider_fixture_id TEXT NOT NULL,
        team_id INTEGER NOT NULL,
        player_id TEXT NOT NULL,
        player TEXT NOT NULL,
        absence_type TEXT,
        reason TEXT,
        fetched_at TEXT NOT NULL,
        PRIMARY KEY(provider_fixture_id, team_id, player_id)
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS provider_payloads (
        provider_fixture_id TEXT NOT NULL,
        payload_kind TEXT NOT NULL,
        payload_json TEXT NOT NULL,
        fetched_at TEXT NOT NULL,
        PRIMARY KEY(provider_fixture_id, payload_kind)
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS sync_runs (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        started_at TEXT NOT NULL,
        finished_at TEXT NOT NULL,
        status TEXT NOT NULL,
        requests_used INTEGER NOT NULL,
        fixtures_seen INTEGER NOT NULL,
        fixtures_mapped INTEGER NOT NULL,
        message TEXT
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS public_sync_runs (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        source TEXT NOT NULL,
        started_at TEXT NOT NULL,
        finished_at TEXT NOT NULL,
        status TEXT NOT NULL,
        requests_used INTEGER NOT NULL DEFAULT 0,
        fixtures_seen INTEGER NOT NULL DEFAULT 0,
        fixtures_mapped INTEGER NOT NULL DEFAULT 0,
        results_imported INTEGER NOT NULL DEFAULT 0,
        odds_rows INTEGER NOT NULL DEFAULT 0,
        quota_remaining INTEGER,
        message TEXT
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS odds_snapshots (
        source TEXT NOT NULL,
        source_url TEXT,
        fixture_id TEXT NOT NULL,
        market_id TEXT NOT NULL,
        selection_id TEXT NOT NULL,
        bookmaker TEXT NOT NULL,
        odds REAL NOT NULL,
        snapshot_kind TEXT NOT NULL,
        snapshot_at TEXT NOT NULL,
        fetched_at TEXT NOT NULL,
        PRIMARY KEY(source, fixture_id, market_id, selection_id, bookmaker, snapshot_kind, snapshot_at)
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS result_provenance (
        fixture_id TEXT PRIMARY KEY,
        source TEXT NOT NULL,
        source_url TEXT,
        source_published_at TEXT,
        fetched_at TEXT NOT NULL
      )")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_provider_lineups_fixture ON provider_lineups(provider_fixture_id)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_provider_absences_fixture ON provider_absences(provider_fixture_id)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_odds_fixture_time ON odds_snapshots(fixture_id, snapshot_at)")
  })
  invisible(TRUE)
}

record_public_sync_run <- function(
    db_path, source, started_at, status, requests_used = 0L, fixtures_seen = 0L,
    fixtures_mapped = 0L, results_imported = 0L, odds_rows = 0L,
    quota_remaining = NA_integer_, message = "") {
  row <- data.frame(
    source = as.character(source),
    started_at = format(started_at, "%Y-%m-%dT%H:%M:%S%z"),
    finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    status = as.character(status),
    requests_used = as.integer(requests_used),
    fixtures_seen = as.integer(fixtures_seen),
    fixtures_mapped = as.integer(fixtures_mapped),
    results_imported = as.integer(results_imported),
    odds_rows = as.integer(odds_rows),
    quota_remaining = as.integer(quota_remaining),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
  with_store(db_path, function(con) DBI::dbAppendTable(con, "public_sync_runs", row))
  invisible(row)
}

latest_public_sync_run <- function(db_path, source = NULL) {
  with_store(db_path, function(con) {
    if (is.null(source)) {
      DBI::dbGetQuery(con, "SELECT * FROM public_sync_runs ORDER BY id DESC LIMIT 1")
    } else {
      DBI::dbGetQuery(
        con, "SELECT * FROM public_sync_runs WHERE source = ? ORDER BY id DESC LIMIT 1",
        params = list(as.character(source))
      )
    }
  }) |>
    tibble::as_tibble()
}

store_odds_snapshots <- function(rows, db_path) {
  if (nrow(rows) == 0) return(0L)
  required <- c(
    "source", "source_url", "fixture_id", "market_id", "selection_id", "bookmaker",
    "odds", "snapshot_kind", "snapshot_at", "fetched_at"
  )
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0) stop("Oran kaydında eksik alanlar: ", paste(missing, collapse = ", "))
  clean <- rows |>
    dplyr::transmute(
      source = as.character(source), source_url = as.character(source_url),
      fixture_id = as.character(fixture_id), market_id = as.character(market_id),
      selection_id = as.character(selection_id), bookmaker = as.character(bookmaker),
      odds = as.numeric(odds), snapshot_kind = as.character(snapshot_kind),
      snapshot_at = as.character(snapshot_at), fetched_at = as.character(fetched_at)
    )
  if (any(!is.finite(clean$odds)) || any(clean$odds <= 1)) stop("Ondalık oranlar 1'den büyük olmalı.")
  with_store(db_path, function(con) {
    DBI::dbWithTransaction(con, for (i in seq_len(nrow(clean))) {
      DBI::dbExecute(con, "
        INSERT INTO odds_snapshots (
          source, source_url, fixture_id, market_id, selection_id, bookmaker,
          odds, snapshot_kind, snapshot_at, fetched_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(source, fixture_id, market_id, selection_id, bookmaker, snapshot_kind, snapshot_at)
        DO UPDATE SET odds=excluded.odds, source_url=excluded.source_url, fetched_at=excluded.fetched_at",
        params = unname(as.list(clean[i, ]))
      )
    })
  })
  nrow(clean)
}

latest_odds_rows <- function(db_path, fixture_id) {
  with_store(db_path, function(con) DBI::dbGetQuery(con, "
    SELECT * FROM odds_snapshots
    WHERE fixture_id = ? AND snapshot_at = (
      SELECT MAX(snapshot_at) FROM odds_snapshots WHERE fixture_id = ?
    )
    ORDER BY market_id, bookmaker, selection_id",
    params = list(as.character(fixture_id), as.character(fixture_id)))) |>
    tibble::as_tibble()
}

store_result_provenance <- function(rows, db_path) {
  if (nrow(rows) == 0) return(0L)
  with_store(db_path, function(con) {
    DBI::dbWithTransaction(con, for (i in seq_len(nrow(rows))) {
      DBI::dbExecute(con, "
        INSERT INTO result_provenance (fixture_id, source, source_url, source_published_at, fetched_at)
        VALUES (?, ?, ?, ?, ?)
        ON CONFLICT(fixture_id) DO UPDATE SET
          source=excluded.source, source_url=excluded.source_url,
          source_published_at=excluded.source_published_at, fetched_at=excluded.fetched_at",
        params = unname(as.list(rows[i, c("fixture_id", "source", "source_url", "source_published_at", "fetched_at")]))
      )
    })
  })
  nrow(rows)
}

store_provider_fixtures <- function(rows, db_path) {
  if (nrow(rows) == 0) return(0L)
  required <- c(
    "provider_fixture_id", "internal_fixture_id", "round", "kickoff", "status_short", "status_long",
    "venue", "home_team_provider", "away_team_provider", "home_goals", "away_goals", "last_synced_at"
  )
  rows <- as.data.frame(rows[, required], stringsAsFactors = FALSE)
  with_store(db_path, function(con) {
    DBI::dbWithTransaction(con, for (i in seq_len(nrow(rows))) {
      DBI::dbExecute(con, "
        INSERT INTO provider_fixtures (
          provider_fixture_id, internal_fixture_id, round, kickoff, status_short, status_long, venue,
          home_team_provider, away_team_provider, home_goals, away_goals, last_synced_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(provider_fixture_id) DO UPDATE SET
          internal_fixture_id=excluded.internal_fixture_id, round=excluded.round, kickoff=excluded.kickoff,
          status_short=excluded.status_short, status_long=excluded.status_long, venue=excluded.venue,
          home_team_provider=excluded.home_team_provider, away_team_provider=excluded.away_team_provider,
          home_goals=excluded.home_goals, away_goals=excluded.away_goals, last_synced_at=excluded.last_synced_at",
        params = unname(as.list(rows[i, ]))
      )
    })
  })
  nrow(rows)
}

replace_provider_snapshot <- function(table, provider_fixture_id, rows, db_path) {
  if (!table %in% c("provider_lineups", "provider_absences")) stop("Geçersiz snapshot tablosu.")
  with_store(db_path, function(con) {
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, paste0("DELETE FROM ", table, " WHERE provider_fixture_id = ?"), params = list(as.character(provider_fixture_id)))
      if (nrow(rows) > 0) DBI::dbAppendTable(con, table, as.data.frame(rows, stringsAsFactors = FALSE))
    })
  })
  nrow(rows)
}

store_provider_payload <- function(provider_fixture_id, kind, payload, db_path, fetched_at = Sys.time()) {
  row <- list(
    as.character(provider_fixture_id), as.character(kind),
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", digits = NA),
    format(fetched_at, "%Y-%m-%dT%H:%M:%S%z")
  )
  with_store(db_path, function(con) DBI::dbExecute(con, "
    INSERT INTO provider_payloads (provider_fixture_id, payload_kind, payload_json, fetched_at)
    VALUES (?, ?, ?, ?)
    ON CONFLICT(provider_fixture_id, payload_kind) DO UPDATE SET
      payload_json=excluded.payload_json, fetched_at=excluded.fetched_at", params = row))
  invisible(TRUE)
}

provider_payload_fetched_at <- function(provider_fixture_id, kind, db_path) {
  value <- with_store(db_path, function(con) DBI::dbGetQuery(con, "
    SELECT fetched_at FROM provider_payloads WHERE provider_fixture_id = ? AND payload_kind = ?",
    params = list(as.character(provider_fixture_id), as.character(kind))))
  if (nrow(value) == 0) return(as.POSIXct(NA))
  parse_model_timestamp(value$fetched_at)[[1]]
}

read_provider_payload <- function(provider_fixture_id, kind, db_path) {
  value <- with_store(db_path, function(con) DBI::dbGetQuery(con, "
    SELECT payload_json FROM provider_payloads WHERE provider_fixture_id = ? AND payload_kind = ?",
    params = list(as.character(provider_fixture_id), as.character(kind))))
  if (nrow(value) == 0) return(NULL)
  jsonlite::fromJSON(value$payload_json[[1]], simplifyVector = FALSE)
}

sync_requests_last_24h <- function(db_path, now = Sys.time()) {
  runs <- with_store(db_path, function(con) DBI::dbGetQuery(con, "SELECT started_at, requests_used FROM sync_runs"))
  if (nrow(runs) == 0) return(0L)
  started <- parse_model_timestamp(runs$started_at)
  as.integer(sum(runs$requests_used[!is.na(started) & started >= now - 24 * 60 * 60], na.rm = TRUE))
}

record_sync_run <- function(db_path, started_at, status, requests_used, fixtures_seen, fixtures_mapped, message = "") {
  row <- data.frame(
    started_at = format(started_at, "%Y-%m-%dT%H:%M:%S%z"),
    finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    status = status,
    requests_used = as.integer(requests_used),
    fixtures_seen = as.integer(fixtures_seen),
    fixtures_mapped = as.integer(fixtures_mapped),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
  with_store(db_path, function(con) DBI::dbAppendTable(con, "sync_runs", row))
  invisible(row)
}

provider_fixture_context <- function(internal_fixture_id, db_path) {
  with_store(db_path, function(con) {
    fixture <- DBI::dbGetQuery(con, "SELECT * FROM provider_fixtures WHERE internal_fixture_id = ?", params = list(as.character(internal_fixture_id)))
    if (nrow(fixture) == 0) return(list(fixture = tibble::tibble(), lineups = tibble::tibble(), absences = tibble::tibble()))
    provider_id <- fixture$provider_fixture_id[[1]]
    list(
      fixture = tibble::as_tibble(fixture),
      lineups = tibble::as_tibble(DBI::dbGetQuery(con, "SELECT * FROM provider_lineups WHERE provider_fixture_id = ?", params = list(provider_id))),
      absences = tibble::as_tibble(DBI::dbGetQuery(con, "SELECT * FROM provider_absences WHERE provider_fixture_id = ?", params = list(provider_id)))
    )
  })
}

automation_health <- function(db_path) {
  with_store(db_path, function(con) {
    last_run <- DBI::dbGetQuery(con, "SELECT * FROM sync_runs ORDER BY id DESC LIMIT 1")
    last_public_run <- DBI::dbGetQuery(con, "SELECT * FROM public_sync_runs ORDER BY id DESC LIMIT 1")
    counts <- DBI::dbGetQuery(con, "
      SELECT
        (SELECT COUNT(*) FROM provider_fixtures) AS fixtures,
        (SELECT COUNT(DISTINCT fixture_id) FROM odds_snapshots) AS odds_matches,
        (SELECT COUNT(*) FROM odds_snapshots) AS odds_rows,
        (SELECT COUNT(DISTINCT provider_fixture_id) FROM provider_lineups WHERE is_starting = 1) AS lineup_matches,
        (SELECT COUNT(*) FROM provider_absences) AS absences,
        (SELECT COUNT(*) FROM postmatch_results) AS results,
        (SELECT COUNT(*) FROM result_provenance) AS sourced_results,
        (SELECT COUNT(DISTINCT provider_fixture_id) FROM provider_payloads WHERE payload_kind IN ('statistics','events','players')) AS detailed_matches")
    list(
      last_run = tibble::as_tibble(last_run),
      last_public_run = tibble::as_tibble(last_public_run),
      counts = tibble::as_tibble(counts)
    )
  })
}

analysis_exists_for_fixture <- function(fixture_id, db_path) {
  with_store(db_path, function(con) {
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM analyses WHERE fixture_id = ?", params = list(as.character(fixture_id)))$n[[1]] > 0
  })
}

record_analysis <- function(prediction, db_path) {
  row <- data.frame(
    fixture_id = prediction$fixture$fixture_id,
    home_team = prediction$home$team,
    away_team = prediction$away$team,
    home_win = unname(prediction$outcomes[["home"]]),
    draw = unname(prediction$outcomes[["draw"]]),
    away_win = unname(prediction$outcomes[["away"]]),
    expected_home_goals = unname(prediction$expected_goals[["home"]]),
    expected_away_goals = unname(prediction$expected_goals[["away"]]),
    model_version = prediction$model_version,
    data_mode = prediction$data_mode %||% if (prediction$is_demo) "demo" else "live",
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    stringsAsFactors = FALSE
  )
  with_store(db_path, function(con) DBI::dbAppendTable(con, "analyses", row))
  invisible(row)
}

analysis_history <- function(db_path, limit = 20L) {
  with_store(db_path, function(con) {
    DBI::dbGetQuery(con, paste0(
      "SELECT fixture_id, home_team, away_team, home_win, draw, away_win, ",
      "model_version, data_mode, created_at FROM analyses ORDER BY id DESC LIMIT ",
      as.integer(limit)
    ))
  })
}

validate_postmatch_upload <- function(df) {
  required <- c("fixture_id", "match_date", "home_team", "away_team", "home_goals", "away_goals")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) stop("Eksik sütunlar: ", paste(missing, collapse = ", "))

  optional <- c(home_xg = NA_real_, away_xg = NA_real_, home_cards = NA_integer_, away_cards = NA_integer_)
  for (name in names(optional)) if (!name %in% names(df)) df[[name]] <- optional[[name]]

  clean <- df |>
    dplyr::transmute(
      fixture_id = as.character(fixture_id),
      match_date = as.character(match_date),
      home_team = as.character(home_team),
      away_team = as.character(away_team),
      home_goals = as.integer(home_goals),
      away_goals = as.integer(away_goals),
      home_xg = as.numeric(home_xg),
      away_xg = as.numeric(away_xg),
      home_cards = as.integer(home_cards),
      away_cards = as.integer(away_cards),
      imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    )
  if (any(!nzchar(clean$fixture_id)) || any(!nzchar(clean$home_team)) || any(!nzchar(clean$away_team))) {
    stop("Fixture ID ve takım adları boş olamaz.")
  }
  if (anyNA(clean$home_goals) || anyNA(clean$away_goals) || any(clean$home_goals < 0) || any(clean$away_goals < 0)) {
    stop("Gol sayıları sıfır veya daha büyük tam sayı olmalı.")
  }
  clean
}

import_postmatch_results <- function(df, db_path) {
  fetched_default <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  provenance <- data.frame(
    fixture_id = as.character(df$fixture_id),
    source = if ("result_source" %in% names(df)) as.character(df$result_source) else "Elle / CSV",
    source_url = if ("source_url" %in% names(df)) as.character(df$source_url) else "",
    source_published_at = if ("source_published_at" %in% names(df)) as.character(df$source_published_at) else "",
    fetched_at = if ("fetched_at" %in% names(df)) as.character(df$fetched_at) else fetched_default,
    stringsAsFactors = FALSE
  )
  clean <- validate_postmatch_upload(df)
  if (exists("validate_super_lig_results", mode = "function")) clean <- validate_super_lig_results(clean)
  with_store(db_path, function(con) {
    DBI::dbWithTransaction(con, {
      for (i in seq_len(nrow(clean))) {
        DBI::dbExecute(con, "
          INSERT INTO postmatch_results (
            fixture_id, match_date, home_team, away_team, home_goals, away_goals,
            home_xg, away_xg, home_cards, away_cards, imported_at
          ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
          ON CONFLICT(fixture_id) DO UPDATE SET
            match_date=excluded.match_date, home_team=excluded.home_team,
            away_team=excluded.away_team, home_goals=excluded.home_goals,
            away_goals=excluded.away_goals,
            home_xg=COALESCE(excluded.home_xg, postmatch_results.home_xg),
            away_xg=COALESCE(excluded.away_xg, postmatch_results.away_xg),
            home_cards=COALESCE(excluded.home_cards, postmatch_results.home_cards),
            away_cards=COALESCE(excluded.away_cards, postmatch_results.away_cards),
            imported_at=excluded.imported_at",
          params = unname(as.list(clean[i, ]))
        )
      }
    })
  })
  store_result_provenance(provenance, db_path)
  nrow(clean)
}

apply_postmatch_learning <- function(data, db_path) {
  results <- with_store(db_path, function(con) {
    DBI::dbGetQuery(con, "SELECT * FROM postmatch_results ORDER BY match_date DESC")
  })
  known_teams <- data$teams$team
  results <- results |>
    dplyr::filter(home_team %in% known_teams, away_team %in% known_teams)
  if (nrow(results) == 0) {
    data$learning_matches <- 0L
    return(data)
  }

  team_rows <- dplyr::bind_rows(
    results |>
      dplyr::transmute(
        team = home_team,
        goals_for = home_goals,
        goals_against = away_goals,
        xg_for = dplyr::coalesce(home_xg, as.numeric(home_goals)),
        cards = dplyr::coalesce(home_cards, 0L)
      ),
    results |>
      dplyr::transmute(
        team = away_team,
        goals_for = away_goals,
        goals_against = home_goals,
        xg_for = dplyr::coalesce(away_xg, as.numeric(away_goals)),
        cards = dplyr::coalesce(away_cards, 0L)
      )
  ) |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      learning_n = dplyr::n(),
      learned_attack = clamp(42 + 18 * mean(xg_for, na.rm = TRUE), 35, 94),
      learned_defence = clamp(89 - 17 * mean(goals_against, na.rm = TRUE), 35, 94),
      learned_discipline = clamp(92 - 9 * mean(cards, na.rm = TRUE), 35, 94),
      .groups = "drop"
    ) |>
    dplyr::mutate(learning_weight = pmin(.35, .05 * learning_n))

  data$teams <- data$teams |>
    dplyr::left_join(team_rows, by = "team") |>
    dplyr::mutate(
      learning_weight = dplyr::coalesce(learning_weight, 0),
      attack = round((1 - learning_weight) * attack + learning_weight * dplyr::coalesce(learned_attack, attack)),
      defence = round((1 - learning_weight) * defence + learning_weight * dplyr::coalesce(learned_defence, defence)),
      discipline = round((1 - learning_weight) * discipline + learning_weight * dplyr::coalesce(learned_discipline, discipline))
    ) |>
    dplyr::select(-dplyr::any_of(c("learning_n", "learned_attack", "learned_defence", "learned_discipline", "learning_weight")))

  data$learning_matches <- nrow(results)
  data
}

parse_model_timestamp <- function(values, date_at_end_of_day = FALSE) {
  parsed <- vapply(as.character(values), function(value) {
    value <- trimws(value)
    if (date_at_end_of_day && grepl("^\\d{4}-\\d{2}-\\d{2}$", value)) {
      value <- paste0(value, "T23:59:59+0300")
    }
    formats <- c("%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
    for (format in formats) {
      candidate <- suppressWarnings(as.POSIXct(value, format = format, tz = "Europe/Istanbul"))
      if (!is.na(candidate)) return(as.numeric(candidate))
    }
    NA_real_
  }, numeric(1))
  as.POSIXct(parsed, origin = "1970-01-01", tz = "Europe/Istanbul")
}

eligible_prediction_results <- function(db_path) {
  with_store(db_path, function(con) {
    results <- DBI::dbGetQuery(con, "SELECT * FROM postmatch_results ORDER BY match_date")
    provenance <- DBI::dbGetQuery(con, "SELECT fixture_id, source AS result_source FROM result_provenance")
    if (nrow(provenance) > 0) results <- dplyr::left_join(results, provenance, by = "fixture_id")
    if (!"result_source" %in% names(results)) results$result_source <- rep(NA_character_, nrow(results))
    analyses <- DBI::dbGetQuery(con, "SELECT * FROM analyses ORDER BY created_at")
    if (nrow(results) == 0 || nrow(analyses) == 0) return(tibble::tibble())
    analyses |>
      dplyr::inner_join(results, by = "fixture_id") |>
      dplyr::mutate(
        analysis_time = parse_model_timestamp(created_at),
        result_time = parse_model_timestamp(match_date, date_at_end_of_day = TRUE)
      ) |>
      dplyr::filter(!is.na(analysis_time), !is.na(result_time), analysis_time <= result_time) |>
      dplyr::group_by(fixture_id) |>
      dplyr::slice_max(analysis_time, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        actual = dplyr::case_when(home_goals > away_goals ~ "home", home_goals < away_goals ~ "away", TRUE ~ "draw"),
        predicted = dplyr::case_when(
          home_win >= draw & home_win >= away_win ~ "home",
          away_win >= home_win & away_win >= draw ~ "away",
          TRUE ~ "draw"
        ),
        correct = predicted == actual,
        predicted_probability = pmax(home_win, draw, away_win),
        p_actual = dplyr::case_when(actual == "home" ~ home_win, actual == "draw" ~ draw, TRUE ~ away_win),
        brier = ((home_win - (actual == "home"))^2 + (draw - (actual == "draw"))^2 + (away_win - (actual == "away"))^2) / 3
      )
  })
}

model_scorecard <- function(db_path) {
  joined <- eligible_prediction_results(db_path)
  if (nrow(joined) == 0) {
    return(tibble::tibble(metric = c("1X2 isabeti", "Brier skoru", "Log loss", "Kapsanan maç"), value = c(NA, NA, NA, 0)))
  }
  tibble::tibble(
    metric = c("1X2 isabeti", "Brier skoru", "Log loss", "Kapsanan maç"),
    value = c(mean(joined$correct), mean(joined$brier), mean(-log(pmax(joined$p_actual, 1e-6))), nrow(joined))
  )
}

prediction_result_history <- function(db_path, limit = 20L) {
  outcome_label <- c(home = "Ev", draw = "Beraberlik", away = "Deplasman")
  joined <- eligible_prediction_results(db_path)
  if (nrow(joined) == 0) return(tibble::tibble())
  joined |>
    dplyr::arrange(dplyr::desc(result_time)) |>
    dplyr::slice_head(n = as.integer(limit)) |>
    dplyr::transmute(
      Maç = paste(home_team.x, "—", away_team.x),
      Tahmin = unname(outcome_label[predicted]),
      Gerçek = unname(outcome_label[actual]),
      Doğru = ifelse(correct, "Evet", "Hayır"),
      `Gerçeğe verilen olasılık` = scales::percent(p_actual, accuracy = .1),
      Brier = format(round(brier, 3), nsmall = 3),
      Kaynak = dplyr::coalesce(result_source, "Bilinmiyor"),
      `Tahmin zamanı` = format(analysis_time, "%d.%m.%Y %H:%M")
    )
}

scorecard_interpretation <- function(db_path) {
  score <- model_scorecard(db_path)
  n <- as.integer(score$value[score$metric == "Kapsanan maç"])
  if (n == 0L) return("Henüz sonuçtan önce kaydedilmiş bir tahmin ile gerçek sonuç eşleşmedi.")
  if (n < 30L) return(paste0(n, " maç var: doğruluk değeri henüz çok oynak; karar vermek için erken."))
  if (n < 100L) return(paste0(n, " maç var: ilk kalibrasyon sinyali oluştu, ama güvenilir kıyas için örneklem hâlâ sınırlı."))
  paste0(n, " maçlık ileriye dönük örneklem: 1X2 isabetiyle birlikte Brier ve log loss'u da izlemek gerekir.")
}
