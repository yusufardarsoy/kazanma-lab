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
  expect_equal(clean$fixture_id, "DEMO-001")
})
