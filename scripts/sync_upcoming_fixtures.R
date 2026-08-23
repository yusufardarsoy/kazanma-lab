source("R/config.R")
source("R/provider_api_football.R")

config <- read_app_config()
fixtures <- fetch_upcoming_fixtures(config, next_n = 20L)
dir.create("data/cache", recursive = TRUE, showWarnings = FALSE)
saveRDS(fixtures, "data/cache/upcoming_fixtures.rds")
message(nrow(fixtures), " yaklaşan maç kaydedildi.")

