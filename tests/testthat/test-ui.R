test_that("authenticated dashboard renders the Super Lig workspace", {
  html <- htmltools::renderTags(dashboard_ui(read_app_config()))$html
  expect_match(html, "Süper Lig DNA", fixed = TRUE)
  expect_match(html, "TFF-2026-W03-09", fixed = TRUE)
  expect_match(html, "Tahmini kaydet", fixed = TRUE)
})

test_that("login opens a working Super Lig dashboard server", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path), add = TRUE)
  config <- read_app_config()
  config$db_path <- db_path
  initialize_store(db_path)

  shiny::testServer(function(input, output, session) app_server(input, output, session, config), {
    session$setInputs(login_submit = 0)
    session$setInputs(login_username = "arda", login_password = "kazanma-lab")
    session$setInputs(login_submit = 1)
    session$flushReact()

    root <- paste(as.character(output$root_ui), collapse = " ")
    header <- paste(as.character(output$match_header), collapse = " ")
    freshness <- paste(as.character(output$freshness_ui), collapse = " ")
    lineup <- paste(as.character(output$home_lineup), collapse = " ")
    expect_match(root, "Maç merkezi", fixed = TRUE)
    expect_match(header, "Gençlerbirliği", fixed = TRUE)
    expect_match(freshness, "TFF fikstürü", fixed = TRUE)
    expect_match(lineup, "Rol bazlı", fixed = TRUE)
    expect_no_error(output$scorecard_table)
    expect_no_error(output$comparison_table)
  })
})
