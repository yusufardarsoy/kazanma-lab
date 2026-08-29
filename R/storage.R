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
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS player_heatmaps (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        player_name TEXT NOT NULL,
        team_name TEXT NOT NULL,
        role TEXT NOT NULL,
        side TEXT,
        defensive_third_pct REAL,
        middle_third_pct REAL,
        attacking_third_pct REAL,
        left_flank_pct REAL,
        right_flank_pct REAL,
        central_pct REAL,
        left_halfspace_pct REAL,
        right_halfspace_pct REAL,
        box_penetration_pct REAL,
        avg_x REAL,
        avg_y REAL,
        total_touches INTEGER,
        image_path TEXT,
        generated_at TEXT NOT NULL,
        UNIQUE(player_name, team_name)
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS tactical_scout_reports (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        match_name TEXT NOT NULL,
        home_team TEXT NOT NULL,
        away_team TEXT NOT NULL,
        home_player TEXT,
        away_player TEXT,
        model_name TEXT NOT NULL,
        report_text TEXT NOT NULL,
        duration_seconds REAL,
        created_at TEXT NOT NULL
      )")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_provider_lineups_fixture ON provider_lineups(provider_fixture_id)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_provider_absences_fixture ON provider_absences(provider_fixture_id)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_odds_fixture_time ON odds_snapshots(fixture_id, snapshot_at)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_player_heatmaps_name ON player_heatmaps(player_name, team_name)")
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

  now <- Sys.time()
  match_times <- parse_model_timestamp(results$match_date)
  results$days_ago <- pmax(0, as.numeric(difftime(now, match_times, units = "days")))
  results$days_ago[is.na(results$days_ago)] <- 7
  results$decay_w <- exp(-0.015 * results$days_ago)

  team_rows <- dplyr::bind_rows(
    results |>
      dplyr::transmute(
        team = home_team,
        goals_for = home_goals,
        goals_against = away_goals,
        xg_for = dplyr::coalesce(home_xg, as.numeric(home_goals)),
        cards = dplyr::coalesce(home_cards, 0L),
        points = dplyr::case_when(home_goals > away_goals ~ 3, home_goals == away_goals ~ 1, TRUE ~ 0),
        decay_w = decay_w
      ),
    results |>
      dplyr::transmute(
        team = away_team,
        goals_for = away_goals,
        goals_against = home_goals,
        xg_for = dplyr::coalesce(away_xg, as.numeric(away_goals)),
        cards = dplyr::coalesce(away_cards, 0L),
        points = dplyr::case_when(away_goals > home_goals ~ 3, away_goals == home_goals ~ 1, TRUE ~ 0),
        decay_w = decay_w
      )
  ) |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      learning_n = dplyr::n(),
      w_xg_for = stats::weighted.mean(xg_for, decay_w, na.rm = TRUE),
      w_goals_for = stats::weighted.mean(goals_for, decay_w, na.rm = TRUE),
      w_goals_against = stats::weighted.mean(goals_against, decay_w, na.rm = TRUE),
      w_cards = stats::weighted.mean(cards, decay_w, na.rm = TRUE),
      w_points = stats::weighted.mean(points, decay_w, na.rm = TRUE),
      learned_attack = clamp(42 + 18 * w_xg_for + 3 * (w_goals_for - w_xg_for), 35, 94),
      learned_defence = clamp(89 - 17 * w_goals_against, 35, 94),
      learned_discipline = clamp(92 - 9 * w_cards, 35, 94),
      learned_transition = clamp(50 + 15 * w_xg_for, 40, 95),
      learned_pressing = clamp(88 - 12 * w_goals_against, 40, 95),
      learned_form_points = w_points,
      learned_1h_ratio = clamp(.42 + .015 * (w_xg_for - 1.3), .36, .52),
      .groups = "drop"
    ) |>
    dplyr::mutate(learning_weight = pmin(.40, .06 * learning_n))

  data$teams <- data$teams |>
    dplyr::left_join(team_rows, by = "team") |>
    dplyr::mutate(
      learning_weight = dplyr::coalesce(learning_weight, 0),
      attack = round((1 - learning_weight) * attack + learning_weight * dplyr::coalesce(learned_attack, attack)),
      defence = round((1 - learning_weight) * defence + learning_weight * dplyr::coalesce(learned_defence, defence)),
      discipline = round((1 - learning_weight) * discipline + learning_weight * dplyr::coalesce(learned_discipline, discipline)),
      transition = round((1 - learning_weight) * transition + learning_weight * dplyr::coalesce(learned_transition, transition)),
      pressing = round((1 - learning_weight) * pressing + learning_weight * dplyr::coalesce(learned_pressing, pressing)),
      prior_ppg = round((1 - learning_weight) * prior_ppg + learning_weight * dplyr::coalesce(learned_form_points, prior_ppg), 2),
      learned_1h_ratio = dplyr::coalesce(learned_1h_ratio, .45)
    ) |>
    dplyr::select(-dplyr::any_of(c("learning_n", "w_xg_for", "w_goals_for", "w_goals_against", "w_cards", "w_points", "learned_attack", "learned_defence", "learned_discipline", "learned_transition", "learned_pressing", "learned_form_points", "learning_weight")))

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
    
    joined <- analyses |>
      dplyr::inner_join(results, by = "fixture_id") |>
      dplyr::mutate(
        analysis_time = parse_model_timestamp(created_at),
        result_time = parse_model_timestamp(match_date, date_at_end_of_day = TRUE)
      ) |>
      dplyr::filter(!is.na(analysis_time), !is.na(result_time), analysis_time <= result_time) |>
      dplyr::group_by(fixture_id) |>
      dplyr::slice_max(analysis_time, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()

    if (nrow(joined) == 0) return(tibble::tibble())

    score_eval <- purrr::map_dfr(seq_len(nrow(joined)), function(i) {
      row <- joined[i, ]
      ehg <- row$expected_home_goals
      eag <- row$expected_away_goals
      m <- score_probability_matrix(ehg, eag, max_goals = 6L)
      top_scores <- top_exact_scores(m, n = 3L)
      htft_res <- half_time_and_htft_probabilities(ehg, eag)

      actual_score_str <- paste0(row$home_goals, "–", row$away_goals)
      pred_score_str <- top_scores$score[[1]]
      top3_scores_vec <- top_scores$score

      exact_match <- (actual_score_str == pred_score_str)
      top3_match <- (actual_score_str %in% top3_scores_vec)
      actual_score_prob <- stats::dpois(row$home_goals, ehg) * stats::dpois(row$away_goals, eag)
      score_loss <- -log(pmax(actual_score_prob, 1e-6))
      goal_err <- (abs(ehg - row$home_goals) + abs(eag - row$away_goals)) / 2

      tibble::tibble(
        pred_top1_score = pred_score_str,
        pred_top1_prob = top_scores$probability[[1]],
        pred_top3_summary = paste0(top_scores$score, " (%", round(top_scores$probability * 100, 1), ")", collapse = ", "),
        pred_htft = htft_res$most_likely_htft$code,
        pred_htft_prob = htft_res$most_likely_htft$probability,
        pred_ht_score = htft_res$most_likely_ht_score$score,
        actual_score = actual_score_str,
        exact_score_match = exact_match,
        top3_score_match = top3_match,
        p_actual_score = actual_score_prob,
        score_log_loss = score_loss,
        goal_mae = goal_err
      )
    })

    joined <- dplyr::bind_cols(joined, score_eval) |>
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
    joined
  })
}

model_scorecard <- function(db_path) {
  joined <- eligible_prediction_results(db_path)
  if (nrow(joined) == 0) {
    return(tibble::tibble(
      metric = c(
        "Tam Skor İsabeti (Top-1)",
        "Top-3 Skor Kapsama Oranı",
        "1X2 isabeti",
        "Ortalama Gol Hatası (MAE)",
        "Skor Log-Loss",
        "Brier skoru",
        "Log loss",
        "Kapsanan maç"
      ),
      value = c(NA, NA, NA, NA, NA, NA, NA, 0)
    ))
  }
  tibble::tibble(
    metric = c(
      "Tam Skor İsabeti (Top-1)",
      "Top-3 Skor Kapsama Oranı",
      "1X2 isabeti",
      "Ortalama Gol Hatası (MAE)",
      "Skor Log-Loss",
      "Brier skoru",
      "Log loss",
      "Kapsanan maç"
    ),
    value = c(
      mean(joined$exact_score_match),
      mean(joined$top3_score_match),
      mean(joined$correct),
      mean(joined$goal_mae),
      mean(joined$score_log_loss),
      mean(joined$brier),
      mean(-log(pmax(joined$p_actual, 1e-6))),
      nrow(joined)
    )
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
      `Gerçek Skor` = actual_score,
      `Model 1. Skoru` = paste0(pred_top1_score, " (%", round(pred_top1_prob * 100, 1), ")"),
      `Top-3 Skor Tahminleri` = pred_top3_summary,
      `Skor İsabeti` = dplyr::case_when(
        exact_score_match ~ "Tam İsabet (1.)",
        top3_score_match ~ "Top-3 İçinde",
        TRUE ~ "Farklı Skor"
      ),
      Doğru = ifelse(correct, "Evet", "Hayır"),
      Tahmin = unname(outcome_label[predicted]),
      Gerçek = unname(outcome_label[actual]),
      `1X2 Tahmin / Gerçek` = paste0(unname(outcome_label[predicted]), " / ", unname(outcome_label[actual])),
      `Tahmin İY/MS` = paste0(pred_htft, " (İY: ", pred_ht_score, ")"),
      `Gol Hatası` = round(goal_mae, 2),
      Brier = format(round(brier, 3), nsmall = 3),
      Kaynak = dplyr::coalesce(result_source, "Bilinmiyor"),
      `Tahmin Zamanı` = format(analysis_time, "%d.%m.%Y %H:%M")
    )
}

scorecard_interpretation <- function(db_path) {
  score <- model_scorecard(db_path)
  n <- as.integer(score$value[score$metric == "Kapsanan maç"])
  if (n == 0L) return("Henüz sonuçtan önce kaydedilmiş bir tahmin ile gerçek sonuç eşleşmedi.")
  if (n < 30L) return(paste0(n, " maç analiz edildi: Tam skor ve İY/MS olasılıkları izleniyor; örneklem arttıkça skor kalibrasyonu netleşecektir."))
  if (n < 100L) return(paste0(n, " maçlık kalibrasyon: Top-3 skor kapsama oranı ve gol MAE değeri modelin skor isabetini yansıtır."))
  paste0(n, " maçlık büyük örneklem: Skor Log-Loss ve Top-3 kapsama oranları üzerinden skor bazlı model ağırlıkları optimize edilmektedir.")
}

team_learning_summary <- function(db_path) {
  base_teams <- if (exists("super_lig_teams", mode = "function")) super_lig_teams() else tibble::tibble()
  if (nrow(base_teams) == 0) return(tibble::tibble())

  results <- with_store(db_path, function(con) {
    DBI::dbGetQuery(con, "SELECT * FROM postmatch_results ORDER BY match_date DESC")
  })
  if (nrow(results) == 0) {
    return(base_teams |>
      dplyr::transmute(
        team_id, team, short, coach,
        matches_played = 0L,
        base_attack = attack, current_attack = attack, attack_delta = 0,
        base_defence = defence, current_defence = defence, defence_delta = 0,
        base_discipline = discipline, current_discipline = discipline, discipline_delta = 0,
        avg_goals_for = NA_real_, avg_goals_against = NA_real_, avg_xg_for = NA_real_, avg_cards = NA_real_,
        learning_weight = 0
      ))
  }

  known_teams <- base_teams$team
  results <- results |>
    dplyr::filter(home_team %in% known_teams, away_team %in% known_teams)

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
      matches_played = dplyr::n(),
      avg_goals_for = round(mean(goals_for, na.rm = TRUE), 2),
      avg_goals_against = round(mean(goals_against, na.rm = TRUE), 2),
      avg_xg_for = round(mean(xg_for, na.rm = TRUE), 2),
      avg_cards = round(mean(cards, na.rm = TRUE), 1),
      pure_learned_attack = round(clamp(42 + 18 * mean(xg_for, na.rm = TRUE) + 3 * (mean(goals_for, na.rm = TRUE) - mean(xg_for, na.rm = TRUE)), 35, 94)),
      pure_learned_defence = round(clamp(89 - 17 * mean(goals_against, na.rm = TRUE), 35, 94)),
      pure_learned_discipline = round(clamp(92 - 9 * mean(cards, na.rm = TRUE), 35, 94)),
      learned_1h_ratio = round(clamp(.42 + .015 * (mean(xg_for, na.rm = TRUE) - 1.3), .36, .52), 3),
      .groups = "drop"
    ) |>
    dplyr::mutate(learning_weight = pmin(.40, .06 * matches_played))

  base_teams |>
    dplyr::left_join(team_rows, by = "team") |>
    dplyr::mutate(
      matches_played = dplyr::coalesce(matches_played, 0L),
      learning_weight = dplyr::coalesce(learning_weight, 0),
      current_attack = round((1 - learning_weight) * attack + learning_weight * dplyr::coalesce(pure_learned_attack, attack)),
      current_defence = round((1 - learning_weight) * defence + learning_weight * dplyr::coalesce(pure_learned_defence, defence)),
      current_discipline = round((1 - learning_weight) * discipline + learning_weight * dplyr::coalesce(pure_learned_discipline, discipline)),
      attack_delta = current_attack - attack,
      defence_delta = current_defence - defence,
      discipline_delta = current_discipline - discipline,
      `1H Tempo Payı` = scales::percent(dplyr::coalesce(learned_1h_ratio, .45), accuracy = .1)
    ) |>
    dplyr::transmute(
      team_id, team, short, coach,
      matches_played,
      base_attack = attack, current_attack, attack_delta,
      base_defence = defence, current_defence, defence_delta,
      base_discipline = discipline, current_discipline, discipline_delta,
      avg_goals_for, avg_goals_against, avg_xg_for, avg_cards,
      `1H Tempo Payı`,
      learning_weight
    ) |>
    dplyr::arrange(dplyr::desc(matches_played), dplyr::desc(current_attack))
}

finished_matches_detailed <- function(db_path, limit = 50L) {
  with_store(db_path, function(con) {
    results <- DBI::dbGetQuery(con, "SELECT * FROM postmatch_results ORDER BY match_date DESC")
    if (nrow(results) == 0) return(tibble::tibble())
    provenance <- DBI::dbGetQuery(con, "SELECT fixture_id, source AS result_source FROM result_provenance")
    if (nrow(provenance) > 0) results <- dplyr::left_join(results, provenance, by = "fixture_id")
    if (!"result_source" %in% names(results)) results$result_source <- rep(NA_character_, nrow(results))

    analyses <- DBI::dbGetQuery(con, "SELECT * FROM analyses ORDER BY created_at")
    if (nrow(analyses) > 0) {
      eligible <- analyses |>
        dplyr::inner_join(results |> dplyr::select(fixture_id, match_date), by = "fixture_id") |>
        dplyr::mutate(
          analysis_time = parse_model_timestamp(created_at),
          result_time = parse_model_timestamp(match_date, date_at_end_of_day = TRUE)
        ) |>
        dplyr::filter(!is.na(analysis_time), !is.na(result_time), analysis_time <= result_time) |>
        dplyr::group_by(fixture_id) |>
        dplyr::slice_max(analysis_time, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::transmute(
          fixture_id,
          pred_home_win = home_win, pred_draw = draw, pred_away_win = away_win,
          pred_exp_hg = expected_home_goals, pred_exp_ag = expected_away_goals,
          pred_outcome = dplyr::case_when(
            home_win >= draw & home_win >= away_win ~ "home",
            away_win >= home_win & away_win >= draw ~ "away",
            TRUE ~ "draw"
          )
        )
      results <- dplyr::left_join(results, eligible, by = "fixture_id")
    } else {
      results$pred_outcome <- rep(NA_character_, nrow(results))
    }

    parsed_date <- parse_model_timestamp(results$match_date)
    outcome_label <- c(home = "Ev", draw = "Beraberlik", away = "Deplasman")
    results |>
      dplyr::mutate(
        actual_outcome = dplyr::case_when(
          home_goals > away_goals ~ "home",
          home_goals < away_goals ~ "away",
          TRUE ~ "draw"
        ),
        is_correct = ifelse(is.na(pred_outcome), NA, pred_outcome == actual_outcome),
        tarih = format(parsed_date, "%d.%m.%Y %H:%M")
      ) |>
      dplyr::arrange(dplyr::desc(parsed_date)) |>
      dplyr::slice_head(n = as.integer(limit)) |>
      dplyr::transmute(
        Tarih = dplyr::coalesce(tarih, match_date),
        Maç = paste(home_team, "—", away_team),
        Skor = paste0(home_goals, " – ", away_goals),
        `xG (Ev-Dep)` = ifelse(is.na(home_xg) | is.na(away_xg), "—", paste0(round(home_xg, 2), " – ", round(away_xg, 2))),
        `Kart (Ev-Dep)` = ifelse(is.na(home_cards) | is.na(away_cards), "—", paste0(home_cards, " – ", away_cards)),
        `Model Tahmini` = ifelse(is.na(pred_outcome), "Tahmin dondurulmadı", unname(outcome_label[pred_outcome])),
        `İsabet` = dplyr::case_when(
          is.na(is_correct) ~ "—",
          is_correct ~ "Evet (Doğru)",
          TRUE ~ "Hayır (Yanlış)"
        ),
        Kaynak = dplyr::coalesce(result_source, "Football-Data.co.uk")
      )
  }) |>
    tibble::as_tibble()
}

match_actual_result <- function(fixture_id, db_path) {
  if (is.null(fixture_id) || !nzchar(fixture_id)) return(NULL)
  with_store(db_path, function(con) {
    res <- DBI::dbGetQuery(con, "SELECT * FROM postmatch_results WHERE fixture_id = ? LIMIT 1", params = list(as.character(fixture_id)))
    if (nrow(res) == 0) return(NULL)
    prov <- DBI::dbGetQuery(con, "SELECT source FROM result_provenance WHERE fixture_id = ? LIMIT 1", params = list(as.character(fixture_id)))
    res$source <- if (nrow(prov) > 0) prov$source[[1]] else "Football-Data.co.uk"
    tibble::as_tibble(res[1, ])
  })
}

save_player_heatmap_db <- function(db_path, heatmap_data) {
  with_store(db_path, function(con) {
    DBI::dbExecute(con, "
      INSERT INTO player_heatmaps (
        player_name, team_name, role, side,
        defensive_third_pct, middle_third_pct, attacking_third_pct,
        left_flank_pct, right_flank_pct, central_pct,
        left_halfspace_pct, right_halfspace_pct, box_penetration_pct,
        avg_x, avg_y, total_touches, image_path, generated_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(player_name, team_name) DO UPDATE SET
        role = excluded.role,
        side = excluded.side,
        defensive_third_pct = excluded.defensive_third_pct,
        middle_third_pct = excluded.middle_third_pct,
        attacking_third_pct = excluded.attacking_third_pct,
        left_flank_pct = excluded.left_flank_pct,
        right_flank_pct = excluded.right_flank_pct,
        central_pct = excluded.central_pct,
        left_halfspace_pct = excluded.left_halfspace_pct,
        right_halfspace_pct = excluded.right_halfspace_pct,
        box_penetration_pct = excluded.box_penetration_pct,
        avg_x = excluded.avg_x,
        avg_y = excluded.avg_y,
        total_touches = excluded.total_touches,
        image_path = excluded.image_path,
        generated_at = excluded.generated_at
    ", params = list(
      as.character(heatmap_data$player_name),
      as.character(heatmap_data$team_name),
      as.character(heatmap_data$role %||% "Winger"),
      as.character(heatmap_data$side %||% "left"),
      as.numeric(heatmap_data$defensive_third_pct %||% 0),
      as.numeric(heatmap_data$middle_third_pct %||% 0),
      as.numeric(heatmap_data$attacking_third_pct %||% 0),
      as.numeric(heatmap_data$left_flank_pct %||% 0),
      as.numeric(heatmap_data$right_flank_pct %||% 0),
      as.numeric(heatmap_data$central_pct %||% 0),
      as.numeric(heatmap_data$left_halfspace_pct %||% 0),
      as.numeric(heatmap_data$right_halfspace_pct %||% 0),
      as.numeric(heatmap_data$box_penetration_pct %||% 0),
      as.numeric(heatmap_data$avg_x %||% 50),
      as.numeric(heatmap_data$avg_y %||% 50),
      as.integer(heatmap_data$total_touches %||% 100),
      as.character(heatmap_data$image_path %||% ""),
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ))
  })
}

get_player_heatmap_db <- function(db_path, player_name, team_name = NULL) {
  with_store(db_path, function(con) {
    query <- if (!is.null(team_name) && nzchar(team_name)) {
      "SELECT * FROM player_heatmaps WHERE player_name = ? AND team_name = ? LIMIT 1"
    } else {
      "SELECT * FROM player_heatmaps WHERE player_name = ? LIMIT 1"
    }
    params <- if (!is.null(team_name) && nzchar(team_name)) list(player_name, team_name) else list(player_name)
    res <- DBI::dbGetQuery(con, query, params = params)
    if (nrow(res) == 0) return(NULL)
    as.list(res[1, ])
  })
}

save_tactical_scout_report_db <- function(db_path, report) {
  with_store(db_path, function(con) {
    DBI::dbExecute(con, "
      INSERT INTO tactical_scout_reports (
        match_name, home_team, away_team, home_player, away_player,
        model_name, report_text, duration_seconds, created_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      as.character(report$match_name %||% ""),
      as.character(report$home_team %||% ""),
      as.character(report$away_team %||% ""),
      as.character(report$home_player %||% ""),
      as.character(report$away_player %||% ""),
      as.character(report$model %||% "meta/llama-3.2-11b-vision-instruct"),
      as.character(report$report_text %||% ""),
      as.numeric(report$duration_seconds %||% 0),
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ))
  })
}

get_latest_tactical_scout_report_db <- function(db_path, match_name = NULL) {
  with_store(db_path, function(con) {
    if (!is.null(match_name) && nzchar(match_name)) {
      query <- "SELECT * FROM tactical_scout_reports WHERE match_name = ? ORDER BY created_at DESC LIMIT 1"
      res <- DBI::dbGetQuery(con, query, params = list(as.character(match_name)))
    } else {
      query <- "SELECT * FROM tactical_scout_reports ORDER BY created_at DESC LIMIT 1"
      res <- DBI::dbGetQuery(con, query)
    }
    if (nrow(res) == 0) return(NULL)
    as.list(res[1, ])
  })
}


