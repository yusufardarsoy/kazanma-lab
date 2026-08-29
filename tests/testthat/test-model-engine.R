test_that("outcome probabilities are valid", {
  prediction <- build_prediction(demo_match_data())
  expect_equal(sum(prediction$outcomes), 1, tolerance = 1e-8)
  expect_true(all(prediction$outcomes >= 0 & prediction$outcomes <= 1))
})

test_that("probable lineups contain eleven players", {
  prediction <- build_prediction(demo_match_data())
  expect_equal(nrow(prediction$home_xi), 11)
  expect_equal(nrow(prediction$away_xi), 11)
  expect_equal(sum(prediction$home_xi$position == "GK"), 1)
})

test_that("player market probabilities remain bounded", {
  prediction <- build_prediction(demo_match_data())
  expect_true(all(prediction$player_markets$scorer_probability >= 0 & prediction$player_markets$scorer_probability <= 1))
  expect_true(all(prediction$player_markets$card_probability >= 0 & prediction$player_markets$card_probability <= 1))
})

test_that("post-match upload validates its contract", {
  template <- testthat::test_path("..", "..", "data", "postmatch_template.csv")
  data <- read.csv(template, stringsAsFactors = FALSE)
  clean <- validate_postmatch_upload(data)
  expect_equal(nrow(clean), 1)
  expect_equal(clean$fixture_id, "317806")
})

test_that("post-match import rejects competitions outside the Super Lig catalog", {
  off_league <- data.frame(
    fixture_id = "UCL-001", match_date = "2026-08-28",
    home_team = "Takım A", away_team = "Takım B",
    home_goals = 1, away_goals = 0
  )
  expect_error(validate_super_lig_results(validate_postmatch_upload(off_league)), "Yalnızca")
})

test_that("Super Lig catalog is complete and exclusive", {
  teams <- super_lig_teams()
  fixtures <- super_lig_fixtures()
  expect_no_error(validate_super_lig_catalog(teams, fixtures))
  expect_equal(nrow(teams), 18)
  expect_equal(dplyr::n_distinct(teams$team), 18)
  expect_true(all(fixtures$competition == SUPER_LIG_COMPETITION))
  expect_equal(nrow(fixtures), 306)
  appearances <- c(fixtures$home_team_id, fixtures$away_team_id)
  expect_true(all(table(appearances) == 34))
  expect_equal(sort(unique(fixtures$round)), 1:34)
})

test_that("every scheduled Super Lig match produces a valid prediction", {
  for (fixture_id in super_lig_fixtures()$fixture_id) {
    prediction <- build_prediction(super_lig_match_data(fixture_id))
    expect_equal(sum(prediction$outcomes), 1, tolerance = 1e-8)
    expect_equal(nrow(prediction$home_xi), 11)
    expect_equal(nrow(prediction$away_xi), 11)
    expect_false(prediction$is_demo)
    expect_true(prediction$data_mode %in% c("curated_prior", "public_schedule", "provider_schedule"))
  }
})

test_that("scorecard compares only a frozen prediction with its later result", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path), add = TRUE)
  initialize_store(db_path)

  prediction <- build_prediction(super_lig_match_data("317808"))
  record_analysis(prediction, db_path)
  predicted <- names(which.max(prediction$outcomes))
  score <- switch(predicted, home = c(1L, 0L), away = c(0L, 1L), draw = c(0L, 0L))
  result <- data.frame(
    fixture_id = prediction$fixture$fixture_id,
    match_date = format(Sys.time() + 3600, "%Y-%m-%dT%H:%M:%S%z"),
    home_team = prediction$home$team,
    away_team = prediction$away$team,
    home_goals = score[[1]],
    away_goals = score[[2]],
    stringsAsFactors = FALSE
  )
  import_postmatch_results(result, db_path)

  scorecard <- model_scorecard(db_path)
  comparison <- prediction_result_history(db_path)
  expect_equal(scorecard$value[scorecard$metric == "Kapsanan maç"], 1)
  expect_equal(scorecard$value[scorecard$metric == "1X2 isabeti"], 1)
  expect_equal(nrow(comparison), 1)
  expect_equal(comparison$Doğru, "Evet")
})

test_that("Kocaelispor-Amed preview uses named probable lineups and match adjustments", {
  prediction <- build_prediction(super_lig_match_data("317800"))
  expect_equal(prediction$fixture$kickoff, as.POSIXct("2026-08-24 21:30:00", tz = "Europe/Istanbul"))
  expect_equal(nrow(prediction$home_xi), 11)
  expect_equal(nrow(prediction$away_xi), 11)
  expect_false(prediction$lineup_role_based)
  expect_true("Daniel Agyei" %in% prediction$home_xi$player)
  expect_true("Gift Orban" %in% prediction$away_xi$player)
  expect_lt(prediction$expected_goals[["home"]], prediction$expected_goals[["away"]])
})

test_that("odds comparison removes margin and flags stale scorer row", {
  prediction <- build_prediction(super_lig_match_data("317800"))
  comparison <- compare_odds(prediction)
  expect_no_error(validate_odds_snapshot())
  expect_true(all(comparison$model_probability[comparison$supported] >= 0))
  expect_true(all(comparison$model_probability[comparison$supported] <= 1))

  exhaustive <- comparison |>
    dplyr::filter(exhaustive) |>
    dplyr::group_by(market_id) |>
    dplyr::summarise(probability_sum = sum(market_probability), .groups = "drop")
  expect_equal(exhaustive$probability_sum, rep(1, nrow(exhaustive)), tolerance = 1e-8)

  double_chance <- comparison |> dplyr::filter(market_id == "double_chance")
  expect_equal(double_chance$market_probability, 1 / double_chance$odds)

  petkovic <- comparison |> dplyr::filter(selection_id == "Bruno Petkovic")
  expect_equal(nrow(petkovic), 1)
  expect_false(petkovic$active)
  expect_equal(petkovic$signal, "Kadro dışı / bayat oran")
})

test_that("team_learning_summary and finished_matches_detailed compute correctly", {
  db_path <- tempfile(fileext = ".sqlite")
  initialize_store(db_path)
  
  # Empty store summary
  empty_summary <- team_learning_summary(db_path)
  expect_equal(nrow(empty_summary), 18)
  expect_true(all(empty_summary$matches_played == 0))
  expect_true(all(empty_summary$attack_delta == 0))
  
  # Add sample match result
  sample_results <- tibble::tibble(
    fixture_id = "317790",
    match_date = "2026-08-14T21:30:00+0300",
    home_team = "Galatasaray",
    away_team = "Çorum FK",
    home_goals = 2L,
    away_goals = 2L,
    home_xg = 2.90,
    away_xg = 0.61,
    home_cards = 1L,
    away_cards = 1L
  )
  import_postmatch_results(sample_results, db_path)
  
  summary <- team_learning_summary(db_path)
  expect_equal(nrow(summary), 18)
  gs <- summary |> dplyr::filter(team == "Galatasaray")
  corum <- summary |> dplyr::filter(team == "Çorum FK")
  
  expect_equal(gs$matches_played, 1)
  expect_equal(corum$matches_played, 1)
  expect_equal(gs$avg_xg_for, 2.90)
  expect_equal(corum$avg_xg_for, 0.61)
  expect_gt(gs$learning_weight, 0)
  
  detailed <- finished_matches_detailed(db_path)
  expect_equal(nrow(detailed), 1)
  expect_equal(detailed$Maç, "Galatasaray — Çorum FK")
  expect_equal(detailed$Skor, "2 – 2")
  expect_equal(detailed$`xG (Ev-Dep)`, "2.9 – 0.61")
  
  # Check plot generation
  plot_obj <- plot_team_learning_evolution(summary)
  expect_s3_class(plot_obj, "ggplot")
})

test_that("exact score ranking and HT/FT probability engine works", {
  m <- score_probability_matrix(1.85, 1.10, max_goals = 6L)
  top_scores <- top_exact_scores(m, n = 8L)
  
  expect_equal(nrow(top_scores), 8)
  expect_true(all(c("score", "probability", "fair_odds", "outcome", "rank") %in% names(top_scores)))
  expect_true(all(top_scores$fair_odds > 1))
  expect_true(all(diff(top_scores$probability) <= 0)) # descending order
  
  htft <- half_time_and_htft_probabilities(1.85, 1.10, 75, 68)
  expect_true(is.list(htft))
  expect_equal(nrow(htft$htft_table), 9)
  expect_true(all(htft$htft_table$code %in% c("0/1", "1/1", "0/0", "1/0", "0/2", "2/2", "2/0", "1/2", "2/1")))
  expect_true(all(htft$htft_table$fair_odds > 1))
  
  # Test predictions containing top_scores and htft
  pred <- build_prediction(super_lig_match_data("317790"))
  expect_true(!is.null(pred$top_scores))
  expect_true(!is.null(pred$htft))
  
  p1 <- plot_top_scores(pred)
  expect_s3_class(p1, "ggplot")
  
  p2 <- plot_htft_probabilities(pred)
  expect_s3_class(p2, "ggplot")
})


