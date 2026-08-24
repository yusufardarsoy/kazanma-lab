clamp <- function(x, lower = 0, upper = 1) pmin(pmax(x, lower), upper)

team_value <- function(team, field, default) {
  if (!field %in% names(team) || length(team[[field]]) == 0 || is.na(team[[field]][[1]])) return(default)
  as.numeric(team[[field]][[1]])
}

team_text <- function(team, field, default = "") {
  if (!field %in% names(team) || length(team[[field]]) == 0 || is.na(team[[field]][[1]])) return(default)
  as.character(team[[field]][[1]])
}

expected_goals <- function(home, away) {
  home_ppg <- 1.35 + team_value(home, "prior_weight", .55) * (team_value(home, "prior_ppg", 1.35) - 1.35)
  away_ppg <- 1.35 + team_value(away, "prior_weight", .55) * (team_value(away, "prior_ppg", 1.35) - 1.35)
  market_delta <- log1p(team_value(home, "market_value_m", 45)) - log1p(team_value(away, "market_value_m", 45))

  home_xg <- 1.45 +
    .020 * (team_value(home, "attack", 72) - 75) -
    .014 * (team_value(away, "defence", 72) - 75) +
    .16 * (home_ppg - away_ppg) +
    .055 * market_delta +
    .004 * (team_value(home, "home_edge", 60) - 60) +
    .0035 * (team_value(home, "transition", 72) + team_value(away, "transition_vulnerability", 55) - 130) +
    .0030 * (team_value(home, "set_piece", 72) + team_value(away, "aerial_vulnerability", 48) - 130) +
    .0020 * (team_value(home, "pressing", 72) + team_value(away, "build_up_vulnerability", 50) - 135)

  away_xg <- 1.16 +
    .020 * (team_value(away, "attack", 72) - 75) -
    .014 * (team_value(home, "defence", 72) - 75) +
    .16 * (away_ppg - home_ppg) -
    .055 * market_delta +
    .0035 * (team_value(away, "transition", 72) + team_value(home, "transition_vulnerability", 55) - 130) +
    .0030 * (team_value(away, "set_piece", 72) + team_value(home, "aerial_vulnerability", 48) - 130) +
    .0020 * (team_value(away, "pressing", 72) + team_value(home, "build_up_vulnerability", 50) - 135)

  c(home = clamp(home_xg, .30, 3.60), away = clamp(away_xg, .25, 3.40))
}

score_probability_matrix <- function(home_xg, away_xg, max_goals = 6L) {
  scores <- 0:max_goals
  matrix(
    outer(dpois(scores, home_xg), dpois(scores, away_xg)),
    nrow = length(scores),
    dimnames = list(home_goals = scores, away_goals = scores)
  )
}

outcome_probabilities <- function(score_matrix) {
  total <- sum(score_matrix)
  c(
    home = sum(score_matrix[row(score_matrix) > col(score_matrix)]) / total,
    draw = sum(diag(score_matrix)) / total,
    away = sum(score_matrix[row(score_matrix) < col(score_matrix)]) / total
  )
}

most_likely_score <- function(score_matrix) {
  idx <- arrayInd(which.max(score_matrix), dim(score_matrix))
  list(home = idx[1] - 1L, away = idx[2] - 1L, probability = max(score_matrix) / sum(score_matrix))
}

select_probable_xi <- function(players) {
  quota <- c(GK = 1L, DEF = 4L, MID = 3L, FWD = 3L)
  players |>
    dplyr::mutate(
      start_probability = clamp(start_score * (.72 + .28 * fitness / 100) * (.82 + .18 * form / 100))
    ) |>
    dplyr::group_by(position) |>
    dplyr::arrange(dplyr::desc(start_probability), .by_group = TRUE) |>
    dplyr::mutate(position_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::filter(position_rank <= unname(quota[position])) |>
    dplyr::arrange(factor(position, levels = c("GK", "DEF", "MID", "FWD")), dplyr::desc(start_probability))
}

project_player_markets <- function(players, team_xg, opponent_discipline) {
  players |>
    dplyr::mutate(
      start_probability = clamp(start_score * (.72 + .28 * fitness / 100) * (.82 + .18 * form / 100)),
      expected_minutes = 90 * minutes_share * (.55 + .45 * start_probability),
      scorer_probability = clamp(1 - exp(-goals_p90 * expected_minutes / 90 * team_xg / 1.45), 0, .78),
      card_probability = clamp(
        1 - exp(-cards_p90 * expected_minutes / 90 * (1 + (65 - opponent_discipline) / 180)),
        0, .68
      )
    )
}

style_long <- function(home, away) {
  keys <- c("pressing", "possession", "directness", "width", "transition", "set_piece", "discipline")
  labels <- c("Pres", "Topa sahip olma", "Dikeylik", "Genişlik", "Geçiş", "Duran top", "Disiplin")

  dplyr::bind_rows(
    tibble::tibble(metric = labels, key = keys, team = home$team, value = as.numeric(unlist(home[1, keys], use.names = FALSE))),
    tibble::tibble(metric = labels, key = keys, team = away$team, value = as.numeric(unlist(away[1, keys], use.names = FALSE)))
  ) |>
    dplyr::mutate(metric = factor(metric, levels = rev(labels)))
}

tactical_notes <- function(home, away) {
  notes <- c(
    glue::glue("{home$team}: {team_text(home, 'strengths', 'iç saha ve hücum eşleşmesi')}. Ana risk: {team_text(home, 'weaknesses', 'veri belirsizliği')}."),
    glue::glue("{away$team}: {team_text(away, 'strengths', 'geçiş ve savunma eşleşmesi')}. Ana risk: {team_text(away, 'weaknesses', 'veri belirsizliği')}.")
  )
  if (team_value(home, "pressing", 70) + team_value(away, "build_up_vulnerability", 50) >= 140) {
    notes <- c(notes, glue::glue("{home$team}, rakibin ilk pasını yüksek presle bozabilecek profile sahip."))
  }
  if (team_value(away, "transition", 70) + team_value(home, "transition_vulnerability", 50) >= 140) {
    notes <- c(notes, glue::glue("{away$team}, savunma arkası koşu ve erken dikey pasla tehdit üretebilir."))
  }
  if (abs(team_value(home, "set_piece", 70) - team_value(away, "set_piece", 70)) >= 8) {
    stronger <- if (home$set_piece > away$set_piece) home$team else away$team
    notes <- c(notes, glue::glue("Duran toplarda belirgin profil üstünlüğü {stronger} tarafında."))
  }
  if (home$discipline < 55 || away$discipline < 55) {
    risky <- if (home$discipline < away$discipline) home$team else away$team
    notes <- c(notes, glue::glue("{risky} için geçiş faulleri ve kart riski maç planını etkileyebilir."))
  }
  if (team_value(home, "profile_confidence", 55) < 60 || team_value(away, "profile_confidence", 55) < 60) {
    notes <- c(notes, "Takımlardan en az birinin profili düşük güvenli: yeni teknik yapı veya büyük kadro değişimi tahmin aralığını büyütüyor.")
  }
  if (length(notes) < 4) {
    notes <- c(notes, "Orta saha ikinci topları ve skorun ilk gol sonrası alacağı yön temel kırılma noktası.")
  }
  utils::head(unique(notes), 5L)
}

build_prediction <- function(data) {
  fixture <- data$fixture[1, ]
  home <- data$teams |> dplyr::filter(team_id == fixture$home_team_id)
  away <- data$teams |> dplyr::filter(team_id == fixture$away_team_id)
  xg <- expected_goals(home, away)
  xg[["home"]] <- clamp(xg[["home"]] + team_value(fixture, "home_xg_adjustment", 0), .30, 3.60)
  xg[["away"]] <- clamp(xg[["away"]] + team_value(fixture, "away_xg_adjustment", 0), .25, 3.40)
  score_matrix <- score_probability_matrix(xg[["home"]], xg[["away"]])
  outcomes <- outcome_probabilities(score_matrix)
  likely_score <- most_likely_score(score_matrix)

  home_players <- data$players |> dplyr::filter(team_id == home$team_id)
  away_players <- data$players |> dplyr::filter(team_id == away$team_id)

  home_markets <- project_player_markets(home_players, xg[["home"]], away$discipline)
  away_markets <- project_player_markets(away_players, xg[["away"]], home$discipline)

  profile_confidence <- mean(c(
    team_value(home, "profile_confidence", 45),
    team_value(away, "profile_confidence", 45)
  )) / 100
  data_mode <- as.character(fixture$data_mode[[1]])

  list(
    fixture = fixture,
    home = home,
    away = away,
    expected_goals = xg,
    score_matrix = score_matrix,
    outcomes = outcomes,
    likely_score = likely_score,
    home_xi = select_probable_xi(home_players),
    away_xi = select_probable_xi(away_players),
    player_markets = dplyr::bind_rows(home_markets, away_markets) |>
      dplyr::left_join(data$teams |> dplyr::select(team_id, team), by = "team_id"),
    styles = style_long(home, away),
    tactical_notes = unique(c(tactical_notes(home, away), team_text(fixture, "adjustment_note", ""))) |> purrr::discard(~ !nzchar(.x)) |> utils::head(5L),
    model_version = "superlig-poisson-prior-0.4",
    confidence = clamp(.34 + .34 * profile_confidence + min(.12, (data$learning_matches %||% 0L) * .006), .42, .80),
    learning_matches = data$learning_matches %||% 0L,
    generated_at = Sys.time(),
    is_demo = identical(data_mode, "demo"),
    data_mode = data_mode,
    lineup_role_based = all(c("identity_status") %in% names(data$players)) && all(data$players$identity_status == "role_prior")
  )
}
