provider_fixture_sample <- function() {
  list(list(
    fixture = list(
      id = 990001L,
      timestamp = as.numeric(as.POSIXct("2026-08-24 21:30:00", tz = "Europe/Istanbul")),
      date = "2026-08-24T21:30:00+03:00",
      status = list(short = "NS", long = "Not Started"),
      venue = list(name = "Kocaeli Stadyumu")
    ),
    league = list(id = 203L, season = 2026L, round = "Regular Season - 3"),
    teams = list(home = list(name = "Kocaelispor"), away = list(name = "Amedspor")),
    goals = list(home = NULL, away = NULL)
  ))
}

provider_lineup_sample <- function() {
  make_team <- function(name, first_id) {
    positions <- c("G", rep("D", 4), rep("M", 3), rep("F", 3))
    starters <- lapply(seq_along(positions), function(i) list(player = list(
      id = first_id + i,
      name = paste(name, "Oyuncu", i),
      number = i,
      pos = positions[[i]],
      grid = paste0(ceiling(i / 3), ":", i)
    )))
    list(team = list(name = name), formation = "4-3-3", coach = list(name = "Teknik Direktör"), startXI = starters, substitutes = list())
  }
  list(make_team("Kocaelispor", 1000L), make_team("Amedspor", 2000L))
}

provider_odds_sample <- function() {
  list(list(
    fixture = list(id = 990001L),
    update = "2026-08-24T15:00:00+00:00",
    bookmakers = list(list(
      name = "Bet365",
      bets = list(
        list(name = "Match Winner", values = list(
          list(value = "Home", odd = "1.97"), list(value = "Draw", odd = "3.05"), list(value = "Away", odd = "2.92")
        )),
        list(name = "Goals Over/Under", values = list(
          list(value = "Over 2.5", odd = "1.88"), list(value = "Under 2.5", odd = "1.82")
        ))
      )
    ))
  ))
}

test_that("provider fixture maps to exactly one internal Super Lig fixture", {
  config <- read_app_config()
  parsed <- parse_provider_fixture_rows(provider_fixture_sample(), config, as.POSIXct("2026-08-24 12:00:00", tz = "Europe/Istanbul"))
  expect_equal(nrow(parsed), 1L)
  expect_equal(parsed$internal_fixture_id, "317800")
  expect_equal(parsed$round, 3L)
})

test_that("official lineups are unique and contain eleven starters per team", {
  parsed <- parse_provider_lineups(provider_lineup_sample(), "990001")
  expect_equal(nrow(parsed), 22L)
  expect_equal(sort(as.integer(table(parsed$team_id))), c(11L, 11L))
  expect_false(anyDuplicated(parsed[c("provider_fixture_id", "team_id", "player_id", "is_starting")]) > 0)
})

test_that("API-Football odds map to the internal fixture and supported markets", {
  config <- read_app_config()
  fixture <- parse_provider_fixture_rows(provider_fixture_sample(), config)
  parsed <- parse_api_football_odds(provider_odds_sample(), fixture)
  expect_equal(nrow(parsed), 5L)
  expect_true(all(parsed$fixture_id == "317800"))
  expect_setequal(parsed$market_id, c("result", "ou_2_5"))
  expect_setequal(parsed$selection_id, c("home", "draw", "away", "over", "under"))
  expect_true(all(parsed$source == "API-Football"))
})

test_that("provider coverage includes pre-match odds", {
  raw <- list(list(seasons = list(list(
    year = 2026L,
    coverage = list(injuries = TRUE, odds = TRUE, fixtures = list(lineups = TRUE))
  ))))
  coverage <- parse_provider_coverage(raw, 2026L)
  expect_true(coverage$odds)
  expect_true(coverage$lineups)
})

test_that("provider storage is idempotent and official lineups reach the model", {
  db_path <- tempfile(fileext = ".sqlite")
  initialize_store(db_path)
  config <- read_app_config()
  fixture <- parse_provider_fixture_rows(provider_fixture_sample(), config)
  lineup <- parse_provider_lineups(provider_lineup_sample(), fixture$provider_fixture_id[[1]])
  store_provider_fixtures(fixture, db_path)
  store_provider_fixtures(fixture, db_path)
  replace_provider_snapshot("provider_lineups", fixture$provider_fixture_id[[1]], lineup, db_path)
  replace_provider_snapshot("provider_lineups", fixture$provider_fixture_id[[1]], lineup, db_path)

  health <- automation_health(db_path)$counts
  expect_equal(health$fixtures, 1L)
  expect_equal(health$lineup_matches, 1L)

  data <- apply_provider_context(super_lig_match_data("317800"), db_path)
  prediction <- build_prediction(data)
  expect_equal(prediction$official_lineup_teams, 2L)
  expect_equal(nrow(prediction$home_xi), 11L)
  expect_equal(nrow(prediction$away_xi), 11L)
  expect_true(all(prediction$home_xi$start_probability == 1))
})

test_that("later score-only updates do not erase saved xG and card data", {
  db_path <- tempfile(fileext = ".sqlite")
  initialize_store(db_path)
  rich <- data.frame(
    fixture_id = "317800", match_date = "2026-08-24T21:30:00+0300",
    home_team = "Kocaelispor", away_team = "Amed SK", home_goals = 1L, away_goals = 2L,
    home_xg = 1.1, away_xg = 1.7, home_cards = 3L, away_cards = 2L
  )
  import_postmatch_results(rich, db_path)
  import_postmatch_results(rich[c("fixture_id", "match_date", "home_team", "away_team", "home_goals", "away_goals")], db_path)
  saved <- with_store(db_path, function(con) DBI::dbGetQuery(con, "SELECT * FROM postmatch_results"))
  expect_equal(saved$home_xg, 1.1)
  expect_equal(saved$away_cards, 2L)
})
