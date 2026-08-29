# tests/testthat/test-agent-intelligence.R

test_that("heatmap generation script produces valid metrics and PNG file", {
  skip_if_not(file.exists("C:/Users/arda/anaconda3/python.exe"), "Python not found")
  
  res <- run_player_heatmap(
    player_name = "Barış Alper Yılmaz",
    team_name = "Galatasaray",
    role = "Sağ Kanat",
    side = "right"
  )
  
  expect_true(is.list(res))
  expect_equal(res$player_name, "Barış Alper Yılmaz")
  expect_true(res$attacking_third_pct > 0)
  expect_true(nzchar(res$image_path))
  expect_true(file.exists(file.path(kazanma_project_root(), "www", res$image_path)))
})

test_that("heatmap DB storage saves and retrieves player heatmaps", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path), add = TRUE)
  initialize_store(db_path)
  
  fake_data <- list(
    player_name = "Mauro Icardi",
    team_name = "Galatasaray",
    role = "Striker",
    side = "center",
    defensive_third_pct = 5.0,
    middle_third_pct = 25.0,
    attacking_third_pct = 70.0,
    left_flank_pct = 10.0,
    right_flank_pct = 10.0,
    central_pct = 80.0,
    left_halfspace_pct = 20.0,
    right_halfspace_pct = 20.0,
    box_penetration_pct = 45.0,
    avg_x = 82.0,
    avg_y = 50.0,
    total_touches = 90L,
    image_path = "heatmaps/test_icardi.png"
  )
  
  save_player_heatmap_db(db_path, fake_data)
  saved <- get_player_heatmap_db(db_path, "Mauro Icardi", "Galatasaray")
  
  expect_false(is.null(saved))
  expect_equal(saved$player_name, "Mauro Icardi")
  expect_equal(saved$box_penetration_pct, 45.0)
})

test_that("tactical scout report DB saves and retrieves latest reports", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path), add = TRUE)
  initialize_store(db_path)
  
  fake_report <- list(
    match_name = "Galatasaray vs Fenerbahçe",
    home_team = "Galatasaray",
    away_team = "Fenerbahçe",
    home_player = "Barış Alper",
    away_player = "Ferdi Kadıoğlu",
    model = "meta/llama-3.2-11b-vision-instruct",
    report_text = "Taktiksel analiz detayları...",
    duration_seconds = 2.4
  )
  
  save_tactical_scout_report_db(db_path, fake_report)
  latest <- get_latest_tactical_scout_report_db(db_path, "Galatasaray vs Fenerbahçe")
  
  expect_false(is.null(latest))
  expect_equal(latest$home_player, "Barış Alper")
  expect_equal(latest$model_name, "meta/llama-3.2-11b-vision-instruct")
})
