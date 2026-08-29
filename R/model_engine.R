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

dixon_coles_tau <- function(x, y, lambda, mu, rho = -0.085) {
  if (x == 0 && y == 0) return(max(0, 1 - lambda * mu * rho))
  if (x == 0 && y == 1) return(max(0, 1 + mu * rho))
  if (x == 1 && y == 0) return(max(0, 1 + lambda * rho))
  if (x == 1 && y == 1) return(max(0, 1 - rho))
  1.0
}

score_probability_matrix <- function(home_xg, away_xg, max_goals = 6L, rho = -0.085) {
  scores <- 0:max_goals
  mat <- matrix(
    outer(dpois(scores, home_xg), dpois(scores, away_xg)),
    nrow = length(scores),
    dimnames = list(home_goals = scores, away_goals = scores)
  )
  for (h in 0:pmin(1L, max_goals)) {
    for (a in 0:pmin(1L, max_goals)) {
      tau <- dixon_coles_tau(h, a, home_xg, away_xg, rho)
      mat[h + 1, a + 1] <- mat[h + 1, a + 1] * tau
    }
  }
  mat / sum(mat)
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

top_exact_scores <- function(score_matrix, n = 10L) {
  df <- as.data.frame(as.table(score_matrix / sum(score_matrix)))
  names(df) <- c("home_goals", "away_goals", "probability")
  df |>
    dplyr::mutate(
      home_goals = as.integer(as.character(home_goals)),
      away_goals = as.integer(as.character(away_goals)),
      score = paste0(home_goals, "–", away_goals),
      fair_odds = round(1 / pmax(probability, 1e-6), 2),
      outcome = dplyr::case_when(
        home_goals > away_goals ~ "Ev Sahibi",
        home_goals < away_goals ~ "Deplasman",
        TRUE ~ "Beraberlik"
      )
    ) |>
    dplyr::arrange(dplyr::desc(probability)) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::slice_head(n = as.integer(n))
}

half_time_and_htft_probabilities <- function(home_xg, away_xg, home_pace = 70, away_pace = 70, home_1h_tempo = NULL, away_1h_tempo = NULL) {
  base_h1_ratio <- if (!is.null(home_1h_tempo) && length(home_1h_tempo) > 0 && !is.na(home_1h_tempo[[1]])) as.numeric(home_1h_tempo[[1]]) else (.45 + .0015 * (as.numeric(home_pace[[1]]) - 70))
  base_a1_ratio <- if (!is.null(away_1h_tempo) && length(away_1h_tempo) > 0 && !is.na(away_1h_tempo[[1]])) as.numeric(away_1h_tempo[[1]]) else (.45 + .0015 * (as.numeric(away_pace[[1]]) - 70))
  
  home_1h_ratio <- clamp(base_h1_ratio, .35, .55)
  away_1h_ratio <- clamp(base_a1_ratio, .35, .55)
  h1 <- home_xg * home_1h_ratio
  a1 <- away_xg * away_1h_ratio
  h2 <- home_xg * (1 - home_1h_ratio)
  a2 <- away_xg * (1 - away_1h_ratio)

  m1 <- score_probability_matrix(h1, a1, max_goals = 5L)
  m2 <- score_probability_matrix(h2, a2, max_goals = 5L)
  ht_outcomes <- outcome_probabilities(m1)

  combos <- c("0/1", "1/1", "0/0", "1/0", "0/2", "2/2", "2/0", "1/2", "2/1")
  combo_labels <- c(
    "0/1" = "İY 0 / MS 1 (İlk Yarı Beraberlik, Maç Sonu Ev)",
    "1/1" = "İY 1 / MS 1 (İlk Yarı Ev, Maç Sonu Ev)",
    "0/0" = "İY 0 / MS 0 (İlk Yarı Beraberlik, Maç Sonu Beraberlik)",
    "1/0" = "İY 1 / MS 0 (İlk Yarı Ev, Maç Sonu Beraberlik)",
    "0/2" = "İY 0 / MS 2 (İlk Yarı Beraberlik, Maç Sonu Deplasman)",
    "2/2" = "İY 2 / MS 2 (İlk Yarı Deplasman, Maç Sonu Deplasman)",
    "2/0" = "İY 2 / MS 0 (İlk Yarı Deplasman, Maç Sonu Beraberlik)",
    "1/2" = "İY 1 / MS 2 (İlk Yarı Ev, Maç Sonu Deplasman)",
    "2/1" = "İY 2 / MS 1 (İlk Yarı Deplasman, Maç Sonu Ev)"
  )
  htft_probs <- stats::setNames(numeric(9), combos)

  for (hg1 in 0:5) {
    for (ag1 in 0:5) {
      p1 <- m1[hg1 + 1, ag1 + 1]
      ht_res <- if (hg1 > ag1) "1" else if (hg1 < ag1) "2" else "0"
      for (hg2 in 0:5) {
        for (ag2 in 0:5) {
          p2 <- m2[hg2 + 1, ag2 + 1]
          ft_hg <- hg1 + hg2
          ft_ag <- ag1 + ag2
          ft_res <- if (ft_hg > ft_ag) "1" else if (ft_hg < ft_ag) "2" else "0"
          combo <- paste0(ht_res, "/", ft_res)
          htft_probs[combo] <- htft_probs[combo] + p1 * p2
        }
      }
    }
  }
  htft_probs <- htft_probs / sum(htft_probs)

  htft_df <- tibble::tibble(
    code = combos,
    label = unname(combo_labels[combos]),
    probability = as.numeric(htft_probs[combos]),
    fair_odds = round(1 / pmax(as.numeric(htft_probs[combos]), 1e-6), 2)
  ) |>
    dplyr::arrange(dplyr::desc(probability)) |>
    dplyr::mutate(rank = dplyr::row_number())

  top_ht_scores <- top_exact_scores(m1, n = 6L)
  most_likely_htft <- htft_df[1, ]
  most_likely_ht <- top_ht_scores[1, ]

  list(
    ht_xg = c(home = h1, away = a1),
    sh_xg = c(home = h2, away = a2),
    ht_matrix = m1,
    ht_outcomes = ht_outcomes,
    top_ht_scores = top_ht_scores,
    htft_table = htft_df,
    most_likely_htft = list(code = most_likely_htft$code[[1]], label = most_likely_htft$label[[1]], probability = most_likely_htft$probability[[1]], fair_odds = most_likely_htft$fair_odds[[1]]),
    most_likely_ht_score = list(score = most_likely_ht$score[[1]], probability = most_likely_ht$probability[[1]], fair_odds = most_likely_ht$fair_odds[[1]])
  )
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

select_match_xi <- function(players) {
  if ("identity_status" %in% names(players) && nrow(players) == 11L && all(players$identity_status == "official_lineup")) {
    return(players |>
      dplyr::mutate(start_probability = 1, position_rank = dplyr::row_number()) |>
      dplyr::arrange(factor(position, levels = c("GK", "DEF", "MID", "FWD"))))
  }
  select_probable_xi(players)
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
  top_scores <- top_exact_scores(score_matrix, 10L)
  htft_data <- half_time_and_htft_probabilities(
    xg[["home"]], xg[["away"]],
    home_pace = home$pressing, away_pace = away$pressing,
    home_1h_tempo = team_value(home, "learned_1h_ratio", NA),
    away_1h_tempo = team_value(away, "learned_1h_ratio", NA)
  )

  home_players <- data$players |> dplyr::filter(team_id == home$team_id)
  away_players <- data$players |> dplyr::filter(team_id == away$team_id)

  home_markets <- project_player_markets(home_players, xg[["home"]], away$discipline)
  away_markets <- project_player_markets(away_players, xg[["away"]], home$discipline)

  profile_confidence <- mean(c(
    team_value(home, "profile_confidence", 45),
    team_value(away, "profile_confidence", 45)
  )) / 100
  data_mode <- as.character(fixture$data_mode[[1]])
  identity_status <- if ("identity_status" %in% names(data$players)) data$players$identity_status else rep("named_probable", nrow(data$players))
  official_lineup_teams <- length(unique(data$players$team_id[identity_status == "official_lineup"]))

  list(
    fixture = fixture,
    home = home,
    away = away,
    expected_goals = xg,
    score_matrix = score_matrix,
    outcomes = outcomes,
    likely_score = likely_score,
    top_scores = top_scores,
    htft = htft_data,
    home_xi = select_match_xi(home_players),
    away_xi = select_match_xi(away_players),
    player_markets = dplyr::bind_rows(home_markets, away_markets) |>
      dplyr::left_join(data$teams |> dplyr::select(team_id, team), by = "team_id"),
    styles = style_long(home, away),
    tactical_notes = unique(c(tactical_notes(home, away), team_text(fixture, "adjustment_note", ""))) |> purrr::discard(~ !nzchar(.x)) |> utils::head(5L),
    model_version = "superlig-poisson-prior-0.4",
    confidence = clamp(
      .34 + .34 * profile_confidence + min(.12, (data$learning_matches %||% 0L) * .006) +
        .03 * official_lineup_teams,
      .42, .84
    ),
    learning_matches = data$learning_matches %||% 0L,
    generated_at = Sys.time(),
    is_demo = identical(data_mode, "demo"),
    data_mode = data_mode,
    lineup_role_based = all(c("identity_status") %in% names(data$players)) && all(data$players$identity_status == "role_prior"),
    official_lineup_teams = official_lineup_teams,
    availability = data$availability %||% tibble::tibble(),
    provider_status = data$provider_status %||% NULL,
    actual_result = data$actual_result %||% NULL
  )
}
