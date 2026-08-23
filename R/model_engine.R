clamp <- function(x, lower = 0, upper = 1) pmin(pmax(x, lower), upper)

expected_goals <- function(home, away) {
  home_xg <- 0.35 +
    0.012 * home$attack + 0.005 * home$transition + 0.004 * home$set_piece -
    0.009 * away$defence + 0.16
  away_xg <- 0.32 +
    0.012 * away$attack + 0.005 * away$directness + 0.004 * away$set_piece -
    0.009 * home$defence

  c(home = clamp(home_xg, .35, 3.4), away = clamp(away_xg, .30, 3.2))
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
  notes <- character()
  if (home$pressing - away$possession >= 15) {
    notes <- c(notes, glue::glue("{home$team}, rakibin ilk pasını yüksek presle bozabilecek profile sahip."))
  }
  if (away$directness - home$defence >= 2) {
    notes <- c(notes, glue::glue("{away$team}, savunma arkası koşu ve erken dikey pasla tehdit üretebilir."))
  }
  if (abs(home$set_piece - away$set_piece) >= 8) {
    stronger <- if (home$set_piece > away$set_piece) home$team else away$team
    notes <- c(notes, glue::glue("Duran toplarda belirgin profil üstünlüğü {stronger} tarafında."))
  }
  if (home$discipline < 55 || away$discipline < 55) {
    risky <- if (home$discipline < away$discipline) home$team else away$team
    notes <- c(notes, glue::glue("{risky} için geçiş faulleri ve kart riski maç planını etkileyebilir."))
  }
  if (length(notes) < 3) {
    notes <- c(notes, "Orta saha ikinci topları ve skorun ilk gol sonrası alacağı yön temel kırılma noktası.")
  }
  unique(notes)
}

build_prediction <- function(data) {
  fixture <- data$fixture[1, ]
  home <- data$teams |> dplyr::filter(team_id == fixture$home_team_id)
  away <- data$teams |> dplyr::filter(team_id == fixture$away_team_id)
  xg <- expected_goals(home, away)
  score_matrix <- score_probability_matrix(xg[["home"]], xg[["away"]])
  outcomes <- outcome_probabilities(score_matrix)
  likely_score <- most_likely_score(score_matrix)

  home_players <- data$players |> dplyr::filter(team_id == home$team_id)
  away_players <- data$players |> dplyr::filter(team_id == away$team_id)

  home_markets <- project_player_markets(home_players, xg[["home"]], away$discipline)
  away_markets <- project_player_markets(away_players, xg[["away"]], home$discipline)

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
    tactical_notes = tactical_notes(home, away),
    model_version = "adaptive-poisson-0.2",
    confidence = clamp(.58 + min(.18, (data$learning_matches %||% 0L) * .012), .58, .76),
    learning_matches = data$learning_matches %||% 0L,
    generated_at = Sys.time(),
    is_demo = identical(fixture$data_mode, "demo")
  )
}
