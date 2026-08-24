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
    expect_equal(prediction$data_mode, "curated_prior")
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
    match_date = format(prediction$fixture$kickoff, "%Y-%m-%dT%H:%M:%S%z"),
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
