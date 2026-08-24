app_server <- function(input, output, session, config) {
  authenticated <- reactiveVal(FALSE)
  login_message <- reactiveVal(NULL)
  failed_logins <- reactiveVal(0L)
  locked_until <- reactiveVal(as.POSIXct(NA))
  default_fixture_id <- unname(super_lig_fixture_choices()[[1]])
  build_selected_prediction <- function(fixture_id = default_fixture_id) {
    build_prediction(apply_postmatch_learning(super_lig_match_data(fixture_id), config$db_path))
  }
  prediction_value <- reactiveVal(build_selected_prediction())
  import_message <- reactiveVal(NULL)
  live_snapshot <- reactiveVal(NULL)

  output$root_ui <- renderUI({
    if (!authenticated()) login_ui(config, login_message()) else dashboard_ui(config)
  })

  observeEvent(input$login_submit, {
    now <- Sys.time()
    lock_time <- locked_until()
    if (!is.na(lock_time) && now < lock_time) {
      minutes_left <- max(1L, ceiling(as.numeric(difftime(lock_time, now, units = "mins"))))
      login_message(paste("Çok fazla hatalı deneme. Tekrar denemek için", minutes_left, "dakika bekle."))
      return()
    }

    if (verify_login(input$login_username, input$login_password, config)) {
      authenticated(TRUE)
      login_message(NULL)
      failed_logins(0L)
      locked_until(as.POSIXct(NA))
    } else {
      next_attempt <- failed_logins() + 1L
      failed_logins(next_attempt)
      if (next_attempt >= 10L) {
        locked_until(now + 20 * 60)
        login_message("Çok fazla hatalı deneme. Giriş 20 dakika kilitlendi.")
      } else {
        login_message(paste0("Kullanıcı adı veya şifre yanlış. Kalan deneme: ", 10L - next_attempt, "."))
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$logout, {
    authenticated(FALSE)
    login_message(NULL)
  }, ignoreInit = TRUE)

  output$section_ui <- renderUI({
    req(authenticated())
    switch(
      input$section %||% "overview",
      overview = overview_ui(),
      lineups = lineups_ui(),
      styles = styles_ui(),
      teams = teams_ui(),
      players = players_ui(),
      memory = memory_ui(),
      overview_ui()
    )
  })

  observeEvent(input$fixture_id, {
    req(authenticated(), input$fixture_id)
    prediction_value(build_selected_prediction(input$fixture_id))
  }, ignoreInit = TRUE)

  observeEvent(input$run_analysis, {
    req(authenticated(), input$fixture_id)
    prediction <- build_selected_prediction(input$fixture_id)
    prediction_value(prediction)
    record_analysis(prediction, config$db_path)
    showNotification("Tahmin zaman damgasıyla donduruldu ve karşılaştırma hafızasına kaydedildi.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$sync_live, {
    req(authenticated(), api_football_enabled(config))
    fixture_id <- trimws(input$live_fixture_id %||% "")
    if (!nzchar(fixture_id)) {
      showNotification("Önce fixture ID gir.", type = "warning")
      return()
    }
    showNotification("Canlı maç özeti alınıyor…", duration = 2)
    tryCatch({
      snapshot <- fetch_fixture_snapshot(config, fixture_id)
      live_snapshot(snapshot)
      showNotification("Fikstür, sağlayıcı tahmini, ilk 11 ve eksik listesi alındı.", type = "message")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)

  prediction <- reactive({
    req(authenticated())
    prediction_value()
  })

  output$freshness_ui <- renderUI({
    p <- prediction()
    div(
      class = "freshness",
      div(class = "sidebar-label", "SON HESAPLAMA"),
      strong(format(p$generated_at, "%d %b · %H:%M")),
      span(p$model_version),
      span(dplyr::case_when(
        identical(p$data_mode, "curated_prior") ~ "TFF fikstürü · küratörlü takım öncülleri",
        p$is_demo ~ "Sentetik veri",
        TRUE ~ "Canlı veri"
      ))
    )
  })

  output$match_header <- renderUI({
    p <- prediction()
    div(
      class = "match-header",
      div(
        div(class = "eyebrow", paste(p$fixture$competition, "·", format(p$fixture$kickoff, "%d %B %Y %H:%M"))),
        h1(paste(p$home$team, "—", p$away$team)),
        p(paste(p$fixture$venue, "· Model güveni", scales::percent(p$confidence, accuracy = 1)))
      ),
      div(
        class = "score-call",
        span("En olası skor"),
        strong(paste0(p$likely_score$home, "–", p$likely_score$away)),
        tags$small(scales::percent(p$likely_score$probability, accuracy = .1))
      )
    )
  })

  output$hero_cards <- renderUI({
    p <- prediction()
    div(
      class = "metric-strip",
      metric_card(paste(p$home$short, "kazanır"), scales::percent(p$outcomes[["home"]], accuracy = 1), paste("Beklenen gol", round(p$expected_goals[["home"]], 2)), "gold"),
      metric_card("Beraberlik", scales::percent(p$outcomes[["draw"]], accuracy = 1), "90 dakika sonucu", "neutral"),
      metric_card(paste(p$away$short, "kazanır"), scales::percent(p$outcomes[["away"]], accuracy = 1), paste("Beklenen gol", round(p$expected_goals[["away"]], 2)), "teal"),
      metric_card("Model güveni", scales::percent(p$confidence, accuracy = 1), "Veri kapsamı + örneklem", "violet")
    )
  })

  output$outcome_plot <- renderPlot(plot_outcomes(prediction()), bg = "transparent", res = 110)
  output$score_plot <- renderPlot(plot_score_matrix(prediction()), bg = "transparent", res = 110)
  output$style_plot <- renderPlot(plot_styles(prediction()), bg = "transparent", res = 110)
  output$scorer_plot <- renderPlot(plot_player_probability(prediction(), "scorer"), bg = "transparent", res = 110)
  output$card_plot <- renderPlot(plot_player_probability(prediction(), "card"), bg = "transparent", res = 110)

  output$tactical_notes <- renderUI({
    div(class = "insight-list", lapply(prediction()$tactical_notes, function(note) div(class = "insight-item", span(class = "insight-dot"), p(note))))
  })

  output$model_note <- renderUI({
    p <- prediction()
    div(
      class = "model-note",
      div(strong("Gol modeli"), span("Poisson + takım gücü + taktik eşleşme")),
      div(strong("11 modeli"), span(if (p$lineup_role_based) "Güncel kadro yok: rol bazlı öncül" else "Başlama × süre × uygunluk")),
      div(strong("Oyuncu modeli"), span(if (p$lineup_role_based) "Oyuncu adı değil, rol olasılığı" else "Dakika ağırlıklı olay oranı")),
      div(strong("Öğrenme hafızası"), span(paste(p$learning_matches, "maç sonucu"))),
      if (!is.null(live_snapshot())) div(strong("Canlı özet"), span("API snapshot hazır; eğitim hattına alınabilir"))
    )
  })

  output$home_lineup <- renderUI(lineup_team_ui(prediction()$home, prediction()$home_xi))
  output$away_lineup <- renderUI(lineup_team_ui(prediction()$away, prediction()$away_xi))

  output$style_cards <- renderUI({
    p <- prediction()
    style_gap <- p$styles |>
      dplyr::select(metric, team, value) |>
      tidyr::pivot_wider(names_from = team, values_from = value) |>
      dplyr::mutate(gap = abs(.data[[p$home$team]] - .data[[p$away$team]])) |>
      dplyr::slice_max(gap, n = 1, with_ties = FALSE)
    press_team <- if (p$home$pressing >= p$away$pressing) p$home else p$away
    set_piece_team <- if (p$home$set_piece >= p$away$set_piece) p$home else p$away
    div(
      class = "style-card-row",
      metric_card("Pres üstünlüğü", press_team$team, paste0(press_team$pressing, "/100"), "gold"),
      metric_card("Duran top üstünlüğü", set_piece_team$team, paste0(set_piece_team$set_piece, "/100"), "teal"),
      metric_card("En büyük stil farkı", as.character(style_gap$metric[[1]]), paste0(style_gap$gap[[1]], " puan"), "violet")
    )
  })

  output$team_profile_table <- renderTable({
    super_lig_profile_table()
  }, striped = FALSE, bordered = FALSE, hover = TRUE, width = "100%", align = "l")

  output$player_table <- renderTable({
    prediction()$player_markets |>
      dplyr::arrange(dplyr::desc(scorer_probability)) |>
      dplyr::slice_head(n = 12) |>
      dplyr::transmute(
        Takım = team,
        Oyuncu = player,
        Pozisyon = position,
        `İlk 11` = scales::percent(start_probability, accuracy = 1),
        `Beklenen dk.` = round(expected_minutes),
        `Gol` = scales::percent(scorer_probability, accuracy = 1),
        `Kart` = scales::percent(card_probability, accuracy = 1)
      )
  }, striped = FALSE, bordered = FALSE, hover = TRUE, width = "100%", align = "l")

  observeEvent(input$import_postmatch, {
    req(authenticated())
    if (is.null(input$postmatch_file)) {
      import_message(list(type = "error", text = "Önce bir CSV seç."))
      return()
    }
    tryCatch({
      df <- utils::read.csv(input$postmatch_file$datapath, check.names = FALSE, stringsAsFactors = FALSE)
      count <- import_postmatch_results(df, config$db_path)
      prediction_value(build_selected_prediction(input$fixture_id %||% default_fixture_id))
      import_message(list(type = "success", text = paste(count, "maç sonucu hafızaya alındı.")))
    }, error = function(e) {
      import_message(list(type = "error", text = conditionMessage(e)))
    })
  }, ignoreInit = TRUE)

  observeEvent(input$save_manual_result, {
    req(authenticated(), input$result_fixture_id)
    tryCatch({
      fixtures <- super_lig_fixtures()
      teams <- super_lig_teams() |> dplyr::select(team_id, team)
      fixture <- fixtures |>
        dplyr::filter(fixture_id == input$result_fixture_id) |>
        dplyr::left_join(teams |> dplyr::rename(home_team_id = team_id, home_team = team), by = "home_team_id") |>
        dplyr::left_join(teams |> dplyr::rename(away_team_id = team_id, away_team = team), by = "away_team_id")
      if (nrow(fixture) != 1L) stop("Seçilen Süper Lig maçı bulunamadı.")
      home_goals <- as.integer(input$manual_home_goals)
      away_goals <- as.integer(input$manual_away_goals)
      if (is.na(home_goals) || is.na(away_goals) || home_goals < 0 || away_goals < 0) stop("Gol sayıları sıfır veya daha büyük olmalı.")
      result <- data.frame(
        fixture_id = fixture$fixture_id,
        match_date = format(fixture$kickoff, "%Y-%m-%dT%H:%M:%S%z"),
        home_team = fixture$home_team,
        away_team = fixture$away_team,
        home_goals = home_goals,
        away_goals = away_goals,
        stringsAsFactors = FALSE
      )
      import_postmatch_results(result, config$db_path)
      prediction_value(build_selected_prediction(input$fixture_id %||% default_fixture_id))
      import_message(list(type = "success", text = paste0(fixture$home_team, " ", home_goals, "–", away_goals, " ", fixture$away_team, " sonucu kaydedildi.")))
    }, error = function(e) {
      import_message(list(type = "error", text = conditionMessage(e)))
    })
  }, ignoreInit = TRUE)

  output$import_status <- renderUI({
    message <- import_message()
    if (is.null(message)) return(NULL)
    div(class = paste("import-message", message$type), message$text)
  })

  output$history_table <- renderTable({
    history <- analysis_history(config$db_path)
    if (nrow(history) == 0) return(data.frame(Bilgi = "Henüz kaydedilmiş analiz yok."))
    history |>
      dplyr::transmute(
        Maç = paste(home_team, "—", away_team),
        `Ev %` = scales::percent(home_win, accuracy = 1),
        `Ber. %` = scales::percent(draw, accuracy = 1),
        `Dep. %` = scales::percent(away_win, accuracy = 1),
        Model = model_version,
        Veri = data_mode,
        Zaman = created_at
      )
  }, width = "100%")

  output$comparison_table <- renderTable({
    comparison <- prediction_result_history(config$db_path)
    if (nrow(comparison) == 0) return(data.frame(Bilgi = "Henüz sonuçtan önce kaydedilmiş tahmin–sonuç eşleşmesi yok."))
    comparison
  }, width = "100%")

  output$scorecard_table <- renderTable({
    score <- model_scorecard(config$db_path)
    score |>
      dplyr::mutate(value = dplyr::case_when(
        metric == "Kapsanan maç" ~ as.character(as.integer(value)),
        metric == "1X2 isabeti" & !is.na(value) ~ scales::percent(value, accuracy = .1),
        is.na(value) ~ "Veri bekleniyor",
        TRUE ~ format(round(value, 3), nsmall = 3)
      )) |>
      dplyr::rename(Metrik = metric, Değer = value)
  }, width = "100%")

  output$accuracy_note <- renderUI({
    div(class = "source-note", strong("Nasıl okunur?"), span(scorecard_interpretation(config$db_path)))
  })
}
