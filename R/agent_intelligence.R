# R/agent_intelligence.R
# AI Tactical Intelligence Agent & Heatmap Engine Bridge

python_executable_path <- function(config = NULL) {
  candidates <- c(
    if (!is.null(config$python_bin) && nzchar(config$python_bin)) config$python_bin,
    "C:/Users/arda/anaconda3/python.exe",
    Sys.which("python"),
    Sys.which("python3")
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  "python"
}

kazanma_project_root <- function() {
  curr <- getwd()
  if (file.exists(file.path(curr, "scripts", "heatmap_engine.py"))) return(curr)
  p1 <- dirname(curr)
  if (file.exists(file.path(p1, "scripts", "heatmap_engine.py"))) return(p1)
  p2 <- dirname(p1)
  if (file.exists(file.path(p2, "scripts", "heatmap_engine.py"))) return(p2)
  curr
}

run_player_heatmap <- function(player_name, team_name = "", role = "Winger", side = "left", config = NULL) {
  py_bin <- python_executable_path(config)
  root <- kazanma_project_root()
  out_dir <- file.path(root, "www", "heatmaps")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  script_path <- file.path(root, "scripts", "heatmap_engine.py")
  args <- c(
    shQuote(script_path),
    "--player", shQuote(as.character(player_name)),
    "--team", shQuote(as.character(team_name)),
    "--role", shQuote(as.character(role)),
    "--side", shQuote(as.character(side)),
    "--outdir", shQuote(out_dir),
    "--json"
  )
  
  res <- tryCatch({
    raw_out <- system2(py_bin, args = args, stdout = TRUE, stderr = TRUE)
    json_str <- paste(raw_out, collapse = "\n")
    json_start <- regexpr("\\{", json_str)[[1]]
    if (json_start > 0) {
      json_str <- substr(json_str, json_start, nchar(json_str))
      jsonlite::fromJSON(json_str)
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
  
  if (is.null(res) || is.null(res$image_path)) {
    return(list(
      player_name = player_name,
      team_name = team_name,
      role = role,
      defensive_third_pct = 20.0,
      middle_third_pct = 45.0,
      attacking_third_pct = 35.0,
      left_flank_pct = if (side == "left") 70.0 else 10.0,
      right_flank_pct = if (side == "right") 70.0 else 10.0,
      central_pct = 20.0,
      left_halfspace_pct = 15.0,
      right_halfspace_pct = 15.0,
      box_penetration_pct = 8.0,
      avg_x = 55.0,
      avg_y = if (side == "left") 75.0 else 25.0,
      total_touches = 100L,
      image_path = "heatmaps/default_heatmap.png"
    ))
  }
  
  if (!is.null(config$db_path)) {
    save_player_heatmap_db(config$db_path, res)
  }
  
  res
}

run_nvidia_ai_scout <- function(
  match_name,
  home_team,
  away_team,
  home_player,
  home_role,
  away_player,
  away_role,
  home_metrics = NULL,
  away_metrics = NULL,
  config = NULL
) {
  py_bin <- python_executable_path(config)
  root <- kazanma_project_root()
  script_path <- file.path(root, "scripts", "nvidia_tactical_agent.py")
  
  args <- c(
    shQuote(script_path),
    "--match", shQuote(as.character(match_name)),
    "--home", shQuote(as.character(home_team)),
    "--away", shQuote(as.character(away_team)),
    "--home_player", shQuote(as.character(home_player)),
    "--home_role", shQuote(as.character(home_role)),
    "--away_player", shQuote(as.character(away_player)),
    "--away_role", shQuote(as.character(away_role)),
    "--json"
  )
  
  res <- tryCatch({
    raw_out <- system2(py_bin, args = args, stdout = TRUE, stderr = TRUE)
    json_str <- paste(raw_out, collapse = "\n")
    json_start <- regexpr("\\{", json_str)[[1]]
    if (json_start > 0) {
      json_str <- substr(json_str, json_start, nchar(json_str))
      parsed <- jsonlite::fromJSON(json_str)
      parsed
    } else {
      list(status = "error", error_message = "JSON çıktısı ayrıştırılamadı.")
    }
  }, error = function(e) {
    list(status = "error", error_message = conditionMessage(e))
  })
  
  if (identical(res$status, "success") && !is.null(config$db_path)) {
    save_tactical_scout_report_db(config$db_path, res)
  }
  
  res
}
