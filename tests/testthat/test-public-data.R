test_that("Football-Data team names and London times map safely to the TFF catalog", {
  sample <- tibble::tibble(
    Div = c("T1", "T1"),
    Date = c("24/08/2026", "23/08/2026"),
    Time = c("19:30", "19:30"),
    HomeTeam = c("Kocaelispor", "Goztep"),
    AwayTeam = c("Amedspor", "Genclerbirligi"),
    FTHG = c(NA, 0), FTAG = c(NA, 1),
    AvgH = c(2.14, 1.62), AvgD = c(3.25, 3.74), AvgA = c(3.18, 4.85),
    `Avg>2.5` = c(1.91, 1.98), `Avg<2.5` = c(1.89, 1.82)
  )
  rows <- parse_football_data_rows(
    sample, as.POSIXct("2026-08-24 05:00:00", tz = "Europe/Istanbul"),
    "https://www.football-data.co.uk/fixtures.csv"
  )
  validate_football_data_rows(rows, "test")
  expect_equal(rows$fixture_id, c("317800", "317797"))
  expect_equal(rows$kickoff[[1]], "2026-08-24T21:30:00+0300")
  expect_false(rows$completed[[1]])
  expect_true(rows$completed[[2]])
})

test_that("public result and odds writes are idempotent and keep provenance", {
  db_path <- tempfile(fileext = ".sqlite")
  initialize_store(db_path)
  result <- data.frame(
    fixture_id = "317790", match_date = "2026-08-14T21:30:00+0300",
    home_team = "Galatasaray", away_team = "Çorum FK",
    home_goals = 2L, away_goals = 2L,
    result_source = FOOTBALL_DATA_SOURCE,
    source_url = "https://www.football-data.co.uk/mmz4281/2627/T1.csv",
    source_published_at = "2026-08-17T15:00:00+0300",
    fetched_at = "2026-08-24T05:00:00+0300",
    stringsAsFactors = FALSE
  )
  expect_equal(import_postmatch_results(result, db_path), 1L)
  expect_equal(import_postmatch_results(result, db_path), 1L)
  saved <- with_store(db_path, function(con) {
    list(
      results = DBI::dbGetQuery(con, "SELECT * FROM postmatch_results"),
      provenance = DBI::dbGetQuery(con, "SELECT * FROM result_provenance")
    )
  })
  expect_equal(nrow(saved$results), 1L)
  expect_equal(saved$provenance$source[[1]], FOOTBALL_DATA_SOURCE)

  odds <- tibble::tibble(
    source = FOOTBALL_DATA_SOURCE,
    source_url = "https://www.football-data.co.uk/fixtures.csv",
    fixture_id = rep("317800", 3), market_id = "result",
    selection_id = c("home", "draw", "away"), bookmaker = "Piyasa ortalaması",
    odds = c(2.14, 3.25, 3.18), snapshot_kind = "upcoming",
    snapshot_at = "2026-08-24T05:00:00+0300", fetched_at = "2026-08-24T05:00:00+0300"
  )
  expect_equal(store_odds_snapshots(odds, db_path), 3L)
  expect_equal(store_odds_snapshots(odds, db_path), 3L)
  expect_equal(nrow(latest_odds_rows(db_path, "317800")), 3L)
})

test_that("The Odds API h2h, totals and completed scores map without provider IDs", {
  raw <- list(list(
    id = "event-1", sport_key = "soccer_turkey_super_league",
    commence_time = "2026-08-24T18:30:00Z",
    home_team = "Kocaelispor", away_team = "Amed Sportif Faaliyetler",
    bookmakers = list(list(
      key = "sample", title = "Sample Book", last_update = "2026-08-24T09:00:00Z",
      markets = list(
        list(key = "h2h", outcomes = list(
          list(name = "Kocaelispor", price = 2.1),
          list(name = "Draw", price = 3.2),
          list(name = "Amed Sportif Faaliyetler", price = 3.3)
        )),
        list(key = "totals", outcomes = list(
          list(name = "Over", price = 1.9, point = 2.5),
          list(name = "Under", price = 1.9, point = 2.5)
        ))
      )
    ))
  ))
  odds <- parse_the_odds_rows(raw, as.POSIXct("2026-08-24 12:05:00", tz = "Europe/Istanbul"))
  expect_equal(nrow(odds), 5L)
  expect_equal(unique(odds$fixture_id), "317800")

  raw[[1]]$completed <- TRUE
  raw[[1]]$scores <- list(
    list(name = "Kocaelispor", score = "1"),
    list(name = "Amed Sportif Faaliyetler", score = "2")
  )
  scores <- parse_the_odds_scores(raw, as.POSIXct("2026-08-24 23:45:00", tz = "Europe/Istanbul"))
  expect_equal(scores$home_goals, 1L)
  expect_equal(scores$away_goals, 2L)
  expect_equal(scores$result_source, THE_ODDS_API_SOURCE)
})
