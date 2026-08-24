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
  })
  invisible(TRUE)
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
            away_goals=excluded.away_goals, home_xg=excluded.home_xg,
            away_xg=excluded.away_xg, home_cards=excluded.home_cards,
            away_cards=excluded.away_cards, imported_at=excluded.imported_at",
          params = unname(as.list(clean[i, ]))
        )
      }
    })
  })
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
