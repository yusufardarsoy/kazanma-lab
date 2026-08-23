test_that("hashed password authentication works", {
  config <- read_app_config()
  config$username <- "private-user"
  config$password <- ""
  config$password_hash <- sodium::password_store("correct-horse-battery-staple")

  expect_true(verify_login("private-user", "correct-horse-battery-staple", config))
  expect_false(verify_login("private-user", "wrong-password", config))
  expect_false(verify_login("someone-else", "correct-horse-battery-staple", config))
})
