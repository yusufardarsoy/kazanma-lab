SUPER_LIG_COMPETITION <- "Trendyol Süper Lig 2026-2027"
SUPER_LIG_PROFILE_DATE <- as.Date("2026-08-24")
SUPER_LIG_PROJECT_ROOT <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(source_file) && nzchar(source_file)) {
    normalizePath(file.path(dirname(source_file), ".."), winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
})

super_lig_teams <- function() {
  # 2025-26 sonuçları TFF'nin resmi final tablosundan; piyasa değerleri
  # Transfermarkt'ın 2026-27 lig katılımcıları sayfasından alınmıştır.
  # Taktik puanlar bu açık veriler ve sezon öncesi takım incelemeleri üzerine
  # kurulmuş, 0-100 arası uzman öncülleridir; gözlenmiş tracking verisi değildir.
  tibble::tribble(
    ~team_id, ~team, ~short, ~coach, ~formation, ~attack, ~defence, ~pressing, ~possession, ~directness, ~width, ~transition, ~set_piece, ~discipline, ~prior_ppg, ~prior_gf_pg, ~prior_ga_pg, ~prior_weight, ~market_value_m, ~squad_depth, ~continuity, ~home_edge, ~transition_vulnerability, ~build_up_vulnerability, ~aerial_vulnerability, ~profile_confidence, ~promoted, ~tactical_identity, ~strengths, ~weaknesses,
    1L, "Galatasaray", "GS", "Okan Buruk", "4-2-3-1", 93, 88, 87, 85, 76, 84, 89, 80, 62, 77/34, 77/34, 30/34, 1.00, 344.75, 94, 86, 76, 39, 30, 35, 88, FALSE, "Yerleşik hücum, önde karşı pres ve kanat-iç koridor rotasyonları", "Ceza sahası hacmi; kadro derinliği; ön alan baskısı", "Bekler ileri çıktığında arkadaki geniş alan; top kaybı geçişleri",
    2L, "Fenerbahçe", "FB", "İsmail Kartal", "4-2-3-1", 92, 84, 84, 82, 82, 86, 90, 84, 58, 74/34, 77/34, 37/34, 1.00, 306.70, 92, 69, 72, 47, 36, 34, 82, FALSE, "Yüksek tempo, erken dikeyleşme ve iki kanadı geniş kullanan hücum", "Geçiş hücumu; hücumcu kalitesi; duran top", "Yeni teknik yapı; hücum kaybı sonrası rest savunması",
    3L, "Beşiktaş", "BJK", "Vincenzo Italiano", "4-3-3", 84, 79, 86, 79, 77, 84, 86, 77, 55, 60/34, 59/34, 40/34, 1.00, 237.00, 87, 60, 68, 50, 43, 41, 73, FALSE, "Ön alan presi, dinamik merkez ve çizgi genişliği", "Baskı şiddeti; bireysel hücum kalitesi; iç saha enerjisi", "Yeni teknik direktör otomasyonları; yüksek çizgi arkası",
    4L, "Trabzonspor", "TS", "Fatih Tekke", "4-2-3-1", 87, 80, 78, 72, 85, 82, 89, 86, 58, 69/34, 61/34, 39/34, 1.00, 155.90, 84, 81, 70, 44, 42, 35, 84, FALSE, "Dikey geçiş, kanat koşuları ve duran top tehdidi", "Hızlı hücum; deplasman üretimi; duran top", "Yerleşik savunmada merkez önü; yüksek tempoda kart riski",
    5L, "İstanbul Başakşehir", "BŞK", "Nuri Şahin", "4-2-3-1", 82, 84, 77, 81, 73, 79, 81, 79, 67, 57/34, 58/34, 35/34, 1.00, 77.13, 77, 74, 61, 41, 34, 38, 80, FALSE, "Kontrollü pas, merkez bağlantıları ve sabırlı yerleşik hücum", "Pas kalitesi; merkez kontrolü; ceza sahası verimliliği", "Maç kaosa döndüğünde geçiş savunması; düşük tempo riski",
    6L, "Göztepe", "GÖZ", "Stanimir Stoilov", "3-4-1-2", 75, 88, 86, 57, 84, 79, 90, 86, 52, 55/34, 42/34, 32/34, 1.00, 61.10, 74, 88, 82, 35, 57, 29, 87, FALSE, "Agresif pres, fiziksel ikili mücadele ve kanat-bek hücumları", "İç saha baskısı; ligin üst düzey savunması; duran top", "Derin savunmaya karşı üretim; yüksek temas nedeniyle kartlar",
    7L, "Samsunspor", "SAM", "Thorsten Fink", "4-2-3-1", 79, 73, 80, 65, 81, 77, 85, 74, 56, 51/34, 46/34, 45/34, 1.00, 47.15, 72, 77, 69, 55, 49, 45, 77, FALSE, "Dengeli blok, ikinci toplar ve hızlı dikey çıkış", "Geçiş hücumu; çalışma temposu; skor çeşitliliği", "Savunma geçişi; yoğun takvimde kadro derinliği",
    8L, "Çorum FK", "ÇOR", "Uğur Uçar", "4-2-3-1", 75, 69, 78, 62, 83, 79, 86, 75, 54, 71/38, 1.65, 0.95, 0.35, 46.88, 68, 38, 71, 57, 55, 49, 57, TRUE, "Enerjik pres, dikey oyun ve hızlı kanat çıkışları", "Geçiş cesareti; hücum temposu; iç saha enerjisi", "On altı yeni transferin uyumu; Süper Lig temposu ve derinlik",
    9L, "Çaykur Rizespor", "RİZ", "Recep Uçar", "4-2-3-1", 75, 72, 82, 63, 80, 78, 84, 78, 50, 41/34, 46/34, 52/34, 1.00, 43.75, 69, 72, 68, 60, 52, 44, 71, FALSE, "Yoğun baskı, dikey pas ve kanat koşuları", "Pres enerjisi; geçiş; iç saha temposu", "Pres arkası alanlar; savunma istikrarı ve disiplin",
    10L, "Alanyaspor", "ALN", "João Pereira", "4-2-3-1", 71, 75, 70, 68, 71, 77, 74, 72, 63, 37/34, 41/34, 41/34, 1.00, 36.58, 67, 75, 62, 48, 43, 47, 75, FALSE, "Esnek yerleşim, kontrollü pas ve sabırlı hücum", "Oyun kontrolü; beraberliği koruma; kanat bağlantıları", "Şansları gole çevirme; maçları kazanacak son aksiyon",
    11L, "Konyaspor", "KON", "İlhan Palut", "4-2-3-1", 70, 76, 73, 63, 78, 76, 77, 84, 56, 40/34, 43/34, 50/34, 1.00, 32.98, 67, 73, 68, 51, 50, 35, 74, FALSE, "Kompakt blok, doğrudan çıkış ve duran top odaklı oyun", "Duran top; savunma organizasyonu; ikinci toplar", "Yerleşik hücumda şans üretimi; geniş alan savunması",
    12L, "Kasımpaşa", "KAS", "Emre Belözoğlu", "4-3-3", 72, 67, 80, 70, 80, 82, 87, 70, 49, 35/34, 33/34, 49/34, 1.00, 29.35, 62, 48, 55, 67, 55, 57, 64, FALSE, "Genç, cesur ön alan baskısı ve çabuk dikeyleşme", "Geçiş hızı; pres cesareti; geniş hücum", "Tecrübe ve kadro derinliği; savunma kutusu; rest savunması",
    13L, "Gaziantep FK", "GFK", "Mirel Rădoi", "4-2-3-1", 70, 67, 72, 57, 85, 70, 87, 82, 47, 37/34, 43/34, 58/34, 1.00, 27.50, 63, 55, 68, 65, 60, 42, 61, FALSE, "Fiziksel, doğrudan ve geçiş odaklı oyun", "Hızlı hücum; hava topu; duran top", "Topa sahipken üretim; savunma istikrarı; kart riski",
    14L, "Amed SK", "AMED", "Besnik Hasi", "4-2-3-1", 75, 74, 78, 61, 82, 78, 86, 80, 52, 74/38, 1.55, 0.95, 0.35, 27.28, 66, 41, 86, 49, 55, 35, 55, TRUE, "Kompakt blok, dikey çıkış ve güçlü iç saha baskısı", "İç saha; geçiş; duran top ve fiziksel direnç", "Yeni teknik direktör ve on altı transferin uyumu; deplasman tecrübesi",
    15L, "Erzurumspor", "ERZ", "Serkan Özbalta", "4-2-3-1", 78, 80, 75, 58, 87, 74, 84, 89, 49, 81/38, 82/38, 27/38, 0.35, 24.38, 65, 86, 89, 43, 60, 29, 62, TRUE, "Kompakt savunma, doğrudan oyun, hava topu ve duran top", "Kadro devamlılığı; hava topları; rakım ve iç saha", "Genişlik savunması; Süper Lig hızına uyum",
    16L, "Gençlerbirliği", "GEN", "Metin Diyadin", "4-2-3-1", 68, 67, 70, 54, 86, 72, 84, 75, 49, 34/34, 36/34, 47/34, 1.00, 23.55, 61, 69, 72, 62, 61, 48, 63, FALSE, "Evde enerjik, doğrudan ve düşük bloktan geçiş oyunu", "İç saha puan üretimi; dikeylik; mücadele", "Deplasman üretimi; kadro derinliği; skor bulma sürekliliği",
    17L, "Kocaelispor", "KOC", "Selçuk İnan", "4-2-3-1", 64, 75, 69, 57, 81, 71, 76, 79, 53, 37/34, 26/34, 38/34, 1.00, 23.15, 62, 73, 74, 50, 63, 42, 69, FALSE, "Kompakt savunma ve doğrudan hücum", "Savunma direnci; iç saha atmosferi; duran top", "Ligin en düşük hücum üretimlerinden biri; merkez yaratıcılığı",
    18L, "Eyüpspor", "EYP", "Özhan Pulat", "4-2-3-1", 66, 64, 68, 62, 78, 74, 80, 71, 49, 33/34, 33/34, 48/34, 1.00, 13.85, 55, 35, 52, 68, 59, 57, 49, FALSE, "Yenilenen kadroyla dengeli pas ve geçiş arayışı", "Teknik oyuncular; geçiş fırsatları", "Büyük kadro devri; savunma; bitiricilik ve derinlik"
  ) |>
    dplyr::mutate(
      form_points = prior_ppg,
      lineup_status = "Rol bazlı ön tahmin",
      profile_as_of = SUPER_LIG_PROFILE_DATE,
      league = SUPER_LIG_COMPETITION
    )
}

super_lig_fixtures <- function() {
  teams <- super_lig_teams() |> dplyr::select(team_id, team)
  raw <- utils::read.csv(
    file.path(SUPER_LIG_PROJECT_ROOT, "data", "super_lig_fixture_catalog.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(fixture_id = as.character(fixture_id), round = as.integer(round))

  schedule <- tibble::tribble(
    ~fixture_id, ~kickoff_text, ~venue,
    "317800", "2026-08-24 21:30:00", "Kocaeli Stadyumu",
    "317806", "2026-08-28 21:30:00", "Eryaman Stadyumu",
    "317807", "2026-08-29 19:00:00", "Konya Büyükşehir Stadyumu",
    "317802", "2026-08-29 21:30:00", "Gaziantep Stadyumu",
    "317808", "2026-08-29 21:30:00", "RAMS Park",
    "317803", "2026-08-30 19:00:00", "Esenyurt Necmi Kadıoğlu Stadyumu",
    "317804", "2026-08-30 21:30:00", "Başakşehir Fatih Terim Stadyumu",
    "317810", "2026-08-30 21:30:00", "Samsun Yeni 19 Mayıs Stadyumu",
    "317805", "2026-08-31 21:30:00", "Diyarbakır Stadyumu",
    "317809", "2026-08-31 21:30:00", "Tüpraş Stadyumu"
  )

  raw <- raw |>
    dplyr::left_join(schedule, by = "fixture_id") |>
    dplyr::mutate(
      home_xg_adjustment = dplyr::if_else(fixture_id == "317800", -0.10, 0),
      away_xg_adjustment = dplyr::if_else(fixture_id == "317800", 0.08, 0),
      adjustment_note = dplyr::if_else(
        fixture_id == "317800",
        "Petković, Jovanović ve Panzo yok; Amed'in 3-0 açılışı düşük örneklem ağırlığıyla işlendi.",
        "Maça özel doğrulanmış eksik/form düzeltmesi henüz yok."
      )
    )

  home_ids <- teams |> dplyr::rename(home_team = team, home_team_id = team_id)
  away_ids <- teams |> dplyr::rename(away_team = team, away_team_id = team_id)
  raw |>
    dplyr::left_join(home_ids, by = "home_team") |>
    dplyr::left_join(away_ids, by = "away_team") |>
    dplyr::transmute(
      fixture_id,
      competition = SUPER_LIG_COMPETITION,
      round,
      kickoff = as.POSIXct(kickoff_text, tz = "Europe/Istanbul"),
      home_team_id,
      away_team_id,
      venue,
      scheduled = !is.na(kickoff),
      home_xg_adjustment,
      away_xg_adjustment,
      adjustment_note,
      data_mode = "curated_prior",
      fixture_source = "TFF"
    ) |>
    dplyr::arrange(dplyr::desc(fixture_id == "317800"), round, kickoff, fixture_id)
}

role_squad_template <- function() {
  tibble::tribble(
    ~slot, ~position, ~role, ~start_score, ~minutes_share, ~goal_factor, ~cards_p90,
    "Kaleci", "GK", "Kaleci", .96, .98, 0.00, .04,
    "Sağ bek", "DEF", "Sağ bek", .89, .84, 0.04, .22,
    "Sağ stoper", "DEF", "Stoper", .93, .91, 0.05, .31,
    "Sol stoper", "DEF", "Stoper", .92, .90, 0.04, .34,
    "Sol bek", "DEF", "Sol bek", .88, .83, 0.05, .24,
    "Yedek savunmacı", "DEF", "Savunma rotasyonu", .42, .35, 0.03, .29,
    "Ön libero", "MID", "Top kazanan orta saha", .92, .89, 0.08, .39,
    "Merkez orta saha", "MID", "İki yönlü orta saha", .89, .85, 0.15, .27,
    "On numara", "MID", "Yaratıcı orta saha", .90, .84, 0.27, .17,
    "Yedek orta saha", "MID", "Orta saha rotasyonu", .45, .38, 0.13, .25,
    "Sağ kanat", "FWD", "İçe kat eden kanat", .88, .82, 0.34, .20,
    "Santrfor", "FWD", "Bitirici santrfor", .94, .89, 0.58, .28,
    "Sol kanat", "FWD", "Geniş hücumcu", .87, .80, 0.32, .17,
    "Yedek forvet", "FWD", "Hücum rotasyonu", .48, .40, 0.43, .14
  )
}

super_lig_role_players <- function(teams = super_lig_teams()) {
  template <- role_squad_template()
  purrr::map_dfr(seq_len(nrow(teams)), function(i) {
    team <- teams[i, ]
    template |>
      dplyr::mutate(
        player_id = as.integer(team$team_id * 100L + dplyr::row_number()),
        team_id = team$team_id,
        player = paste(team$short, "·", slot, "adayı"),
        goals_p90 = pmax(0, goal_factor * (0.78 + team$attack / 330)),
        form = round((team$attack + team$transition) / 2),
        fitness = 90,
        identity_status = "role_prior"
      ) |>
      dplyr::select(player_id, team_id, player, position, role, start_score, minutes_share, goals_p90, cards_p90, form, fitness, identity_status)
  })
}

kocaeli_amed_probable_players <- function() {
  tibble::tribble(
    ~player_id, ~team_id, ~player, ~position, ~role, ~start_score, ~minutes_share, ~goals_p90, ~cards_p90, ~form, ~fitness, ~identity_status,
    17001L, 17L, "Serhat Öztaşdelen", "GK", "Kaleci", .98, .98, .00, .03, 66, 95, "named_probable",
    17002L, 17L, "Tanguy Zoukrou", "DEF", "Sağ bek", .93, .90, .03, .29, 67, 94, "named_probable",
    17003L, 17L, "Anfernee Dijksteel", "DEF", "Stoper", .95, .93, .03, .24, 69, 95, "named_probable",
    17004L, 17L, "Uğur Kaan Yıldız", "DEF", "Stoper", .95, .94, .04, .33, 68, 96, "named_probable",
    17005L, 17L, "Massadio Haïdara", "DEF", "Sol bek", .94, .91, .04, .25, 67, 94, "named_probable",
    17006L, 17L, "Show", "MID", "Top kazanan orta saha", .94, .92, .07, .38, 68, 95, "named_probable",
    17007L, 17L, "Mahamadou Susoho", "MID", "Merkez orta saha", .89, .80, .10, .27, 67, 89, "named_probable",
    17008L, 17L, "Tayfur Bingöl", "MID", "Hücum bağlantısı", .92, .86, .15, .31, 68, 93, "named_probable",
    17009L, 17L, "Makana Baku", "FWD", "Sağ kanat", .94, .86, .28, .17, 69, 94, "named_probable",
    17010L, 17L, "Daniel Agyei", "FWD", "Santrfor", .95, .88, .42, .20, 70, 95, "named_probable",
    17011L, 17L, "Metehan Altunbaş", "FWD", "Sol hücumcu", .91, .81, .36, .16, 68, 94, "named_probable",
    14001L, 14L, "Alban Lafont", "GK", "Kaleci", .98, .98, .00, .02, 76, 97, "named_probable",
    14002L, 14L, "Mehmet Yeşil", "DEF", "Sağ stoper", .94, .92, .04, .31, 75, 96, "named_probable",
    14003L, 14L, "David Bates", "DEF", "Stoper", .95, .94, .05, .30, 76, 96, "named_probable",
    14004L, 14L, "Lumbardh Dellova", "DEF", "Sol stoper", .95, .94, .05, .33, 76, 96, "named_probable",
    14005L, 14L, "Ermal Krasniqi", "DEF", "Kanat bek", .93, .89, .20, .18, 78, 95, "named_probable",
    14006L, 14L, "Rayan Raveloson", "MID", "Top kazanan orta saha", .95, .93, .10, .35, 77, 96, "named_probable",
    14007L, 14L, "Cem Üstündağ", "MID", "Merkez orta saha", .93, .89, .11, .36, 76, 95, "named_probable",
    14008L, 14L, "Dia Saba", "MID", "Yaratıcı orta saha", .95, .90, .25, .16, 80, 96, "named_probable",
    14009L, 14L, "Yira Sor", "FWD", "Sağ hücumcu", .94, .86, .32, .12, 79, 95, "named_probable",
    14010L, 14L, "Gift Orban", "FWD", "Hareketli forvet", .96, .90, .54, .17, 82, 96, "named_probable",
    14011L, 14L, "Mbaye Diagne", "FWD", "Santrfor", .95, .88, .60, .28, 81, 95, "named_probable"
  )
}

validate_super_lig_catalog <- function(teams = super_lig_teams(), fixtures = super_lig_fixtures()) {
  score_fields <- c(
    "attack", "defence", "pressing", "possession", "directness", "width",
    "transition", "set_piece", "discipline", "squad_depth", "continuity",
    "home_edge", "transition_vulnerability", "build_up_vulnerability",
    "aerial_vulnerability", "profile_confidence"
  )
  if (nrow(teams) != 18L || dplyr::n_distinct(teams$team_id) != 18L || dplyr::n_distinct(teams$team) != 18L) {
    stop("Süper Lig takım kataloğu tam olarak 18 benzersiz takım içermeli.")
  }
  if (any(!is.finite(as.matrix(teams[, score_fields]))) || any(as.matrix(teams[, score_fields]) < 0) || any(as.matrix(teams[, score_fields]) > 100)) {
    stop("Takım profil puanları 0-100 aralığında ve eksiksiz olmalı.")
  }
  if (anyNA(fixtures$home_team_id) || anyNA(fixtures$away_team_id) || any(fixtures$home_team_id == fixtures$away_team_id)) {
    stop("Fikstürde takım eşleşme hatası var.")
  }
  if (anyDuplicated(fixtures$fixture_id) || any(fixtures$competition != SUPER_LIG_COMPETITION)) {
    stop("Fikstür kimlikleri benzersiz olmalı ve yalnızca Süper Lig içermeli.")
  }
  invisible(TRUE)
}

validate_super_lig_results <- function(results) {
  teams <- super_lig_teams() |> dplyr::select(team_id, team)
  catalog <- super_lig_fixtures() |>
    dplyr::left_join(teams |> dplyr::rename(home_team_id = team_id, expected_home = team), by = "home_team_id") |>
    dplyr::left_join(teams |> dplyr::rename(away_team_id = team_id, expected_away = team), by = "away_team_id") |>
    dplyr::select(fixture_id, expected_home, expected_away)
  checked <- results |>
    dplyr::left_join(catalog, by = "fixture_id")
  if (anyNA(checked$expected_home) || anyNA(checked$expected_away)) {
    stop("Yalnızca uygulamadaki 2026-27 Süper Lig fixture ID'leri sonuç olarak yüklenebilir.")
  }
  if (any(checked$home_team != checked$expected_home) || any(checked$away_team != checked$expected_away)) {
    stop("Fixture ID ile takım adları eşleşmiyor; sonuç kaydı reddedildi.")
  }
  results
}

super_lig_match_data <- function(fixture_id = NULL) {
  teams <- super_lig_teams()
  fixtures <- super_lig_fixtures()
  validate_super_lig_catalog(teams, fixtures)
  if (is.null(fixture_id) || !nzchar(fixture_id)) fixture_id <- fixtures$fixture_id[[1]]
  fixture <- fixtures |> dplyr::filter(.data$fixture_id == !!fixture_id)
  if (nrow(fixture) != 1L) stop("Seçilen maç 2026-27 Süper Lig fikstüründe bulunamadı.")
  players <- super_lig_role_players(teams)
  if (identical(fixture_id, "317800")) {
    teams <- teams |>
      dplyr::mutate(
        lineup_status = dplyr::if_else(team_id %in% c(14L, 17L), "Maç önü muhtemel 11", lineup_status),
        formation = dplyr::case_when(team_id == 14L ~ "3-4-3", TRUE ~ formation)
      )
    players <- players |>
      dplyr::filter(!team_id %in% c(14L, 17L)) |>
      dplyr::bind_rows(kocaeli_amed_probable_players())
  }
  list(
    teams = teams,
    players = players,
    fixture = fixture,
    recent_matches = tibble::tibble(),
    learning_matches = 0L
  )
}

super_lig_fixture_choices <- function(team_filter = NULL, scheduled_only = FALSE) {
  fixtures <- super_lig_fixtures()
  teams <- super_lig_teams() |> dplyr::select(team_id, team)
  choices <- fixtures |>
    dplyr::left_join(teams |> dplyr::rename(home_team_id = team_id, home_team = team), by = "home_team_id") |>
    dplyr::left_join(teams |> dplyr::rename(away_team_id = team_id, away_team = team), by = "away_team_id")
  if (!is.null(team_filter) && nzchar(team_filter)) {
    team_id <- teams$team_id[match(team_filter, teams$team)]
    choices <- choices |> dplyr::filter(home_team_id == team_id | away_team_id == team_id)
  }
  if (isTRUE(scheduled_only)) choices <- choices |> dplyr::filter(scheduled)
  choices <- choices |>
    dplyr::mutate(
      date_label = dplyr::if_else(scheduled, format(kickoff, "%d.%m %H:%M"), "tarih bekleniyor"),
      label = paste0(round, ". hafta · ", date_label, " · ", home_team, " — ", away_team)
    )
  stats::setNames(choices$fixture_id, choices$label)
}

super_lig_profile_table <- function() {
  super_lig_teams() |>
    dplyr::transmute(
      Takım = team,
      `Teknik direktör` = coach,
      Diziliş = formation,
      `Oyun kimliği` = tactical_identity,
      `Öne çıkan` = strengths,
      `Ana risk` = weaknesses,
      `Veri güveni` = scales::percent(profile_confidence / 100, accuracy = 1)
    )
}
