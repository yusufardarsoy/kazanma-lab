demo_match_data <- function() {
  teams <- tibble::tribble(
    ~team_id, ~team, ~short, ~formation, ~attack, ~defence, ~pressing, ~possession, ~directness, ~width, ~transition, ~set_piece, ~discipline, ~form_points,
    1L, "Boğaz FK", "BĞZ", "4-2-3-1", 78, 72, 84, 66, 57, 73, 82, 70, 61, 2.23,
    2L, "Anadolu 1907", "AND", "4-3-3", 74, 77, 68, 61, 76, 59, 71, 81, 48, 2.08
  )

  players <- tibble::tribble(
    ~player_id, ~team_id, ~player, ~position, ~role, ~start_score, ~minutes_share, ~goals_p90, ~cards_p90, ~form, ~fitness,
    101L,1L,"Mert Kaya","GK","Sweeper keeper",.97,.98,.00,.04,72,99,
    102L,1L,"Emir Arslan","DEF","Overlapping full-back",.91,.88,.04,.18,76,94,
    103L,1L,"Kerem Şahin","DEF","Ball-playing centre-back",.95,.94,.05,.27,79,98,
    104L,1L,"Bora Demir","DEF","Stopper",.89,.86,.03,.39,70,91,
    105L,1L,"Can Eren","DEF","Inverted full-back",.84,.80,.07,.25,74,89,
    106L,1L,"Aras Yıldız","MID","Ball winner",.92,.90,.09,.44,81,96,
    107L,1L,"Ege Aydın","MID","Deep playmaker",.88,.85,.13,.22,78,93,
    108L,1L,"Baran Aksoy","MID","No. 10",.94,.89,.31,.16,86,97,
    109L,1L,"Deniz Kılıç","FWD","Inside forward",.93,.88,.48,.24,84,95,
    110L,1L,"Atlas Koç","FWD","Pressing forward",.96,.92,.64,.31,88,98,
    111L,1L,"Rüzgar Çelik","FWD","Wide creator",.87,.83,.29,.12,77,92,
    112L,1L,"Umut Tunç","DEF","Centre-back",.37,.34,.02,.33,67,96,
    113L,1L,"Tuna Işık","MID","Box-to-box",.43,.39,.18,.29,71,90,
    114L,1L,"Yiğit Sönmez","FWD","Poacher",.46,.42,.55,.09,73,87,
    201L,2L,"Kaan Öz","GK","Line keeper",.98,.99,.00,.03,78,100,
    202L,2L,"Doruk Aslan","DEF","Defensive full-back",.92,.90,.03,.31,80,96,
    203L,2L,"Selim Güneş","DEF","Ball-playing centre-back",.96,.95,.06,.21,83,98,
    204L,2L,"Ozan Kurt","DEF","Stopper",.94,.91,.02,.47,77,95,
    205L,2L,"Poyraz Ateş","DEF","Wing-back",.79,.74,.08,.28,69,84,
    206L,2L,"Ayaz Erdem","MID","Anchor",.95,.93,.06,.38,82,99,
    207L,2L,"Efe Karaca","MID","Box-to-box",.91,.88,.22,.34,80,94,
    208L,2L,"Sarp Yalçın","MID","Advanced playmaker",.86,.82,.27,.19,75,90,
    209L,2L,"Miran Keskin","FWD","Direct winger",.92,.89,.36,.26,81,96,
    210L,2L,"Alp Çınar","FWD","Target forward",.95,.92,.58,.43,85,97,
    211L,2L,"Kuzey Acar","FWD","Inside forward",.83,.78,.41,.17,72,88,
    212L,2L,"Eren Polat","DEF","Full-back",.49,.45,.04,.22,73,95,
    213L,2L,"Berk Ay","MID","Deep playmaker",.41,.37,.12,.20,68,92,
    214L,2L,"Mete Uslu","FWD","Second striker",.51,.46,.46,.14,76,91
  )

  fixture <- tibble::tibble(
    fixture_id = "DEMO-001",
    competition = "Sentetik Süper Lig",
    kickoff = as.POSIXct(paste(Sys.Date() + 1, "20:00:00"), tz = "Europe/Istanbul"),
    home_team_id = 1L,
    away_team_id = 2L,
    venue = "Veri Arena",
    data_mode = "demo"
  )

  recent_matches <- tibble::tibble(
    match = 1:12,
    home_xg = c(1.1,1.8,1.5,2.2,1.6,1.9,2.4,1.7,2.0,1.4,2.1,1.8),
    away_xg = c(1.5,1.2,1.7,1.1,1.4,1.6,1.0,1.8,1.3,1.5,1.2,1.4)
  )

  list(teams = teams, players = players, fixture = fixture, recent_matches = recent_matches, learning_matches = 0L)
}
