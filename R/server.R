app_server <- function(input, output, session, config) {
  authenticated <- reactiveVal(FALSE)
  login_message <- reactiveVal(NULL)
  prediction_value <- reactiveVal(build_prediction(apply_postmatch_learning(demo_match_data(), config$db_path)))
  import_message <- reactiveVal(NULL)
  live_snapshot <- reactiveVal(NULL)

  output$root_ui <- renderUI({
    if (!authenticated()) login_ui(config, login_message()) else dashboard_ui(config)
  })

  observeEvent(input$login_submit, {
    if (verify_login(input$login_username, input$login_password, config)) {
      authenticated(TRUE)
      login_message(NULL)
    } else {
      login_message("Kullanıcı adı veya şifre yanlış.")
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
      players = players_ui(),
      memory = memory_ui(),
      overview_ui()
    )
  })

  observeEvent(input$run_analysis, {
    req(authenticated())
    prediction <- build_prediction(apply_postmatch_learning(demo_match_data(), config$db_path))
    prediction_value(prediction)
    record_analysis(prediction, config$db_path)
    showNotification("Maç yeniden analiz edildi ve hafızaya kaydedildi.", type = "message")
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
      span(if (p$is_demo) "Sentetik veri · gerçek maç iddiası yok" else "Canlı veri")
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
        small(scales::percent(p$likely_score$probability, accuracy = .1))
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
      div(strong("Gol modeli"), span("Poisson skor dağılımı")),
      div(strong("11 modeli"), span("Başlama × süre × uygunluk")),
      div(strong("Oyuncu modeli"), span("Dakika ağırlıklı olay oranı")),
      div(strong("Öğrenme hafızası"), span(paste(p$learning_matches, "maç sonucu"))),
      if (!is.null(live_snapshot())) div(strong("Canlı özet"), span("API snapshot hazır; eğitim hattına alınabilir"))
    )
  })

  output$home_lineup <- renderUI(lineup_team_ui(prediction()$home, prediction()$home_xi))
  output$away_lineup <- renderUI(lineup_team_ui(prediction()$away, prediction()$away_xi))

  output$style_cards <- renderUI({
    p <- prediction()
    home_adv <- p$styles |> dplyr::group_by(metric) |> dplyr::filter(value == max(value)) |> dplyr::slice_head(n = 1) |> dplyr::ungroup()
    div(
      class = "style-card-row",
      metric_card("Pres üstünlüğü", p$home$team, paste0(p$home$pressing, "/100"), "gold"),
      metric_card("Duran top üstünlüğü", p$away$team, paste0(p$away$set_piece, "/100"), "teal"),
      metric_card("En büyük stil farkı", as.character(home_adv$metric[which.max(home_adv$value)]), paste0(max(home_adv$value), "/100"), "violet")
    )
  })

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
      prediction_value(build_prediction(apply_postmatch_learning(demo_match_data(), config$db_path)))
      import_message(list(type = "success", text = paste(count, "maç sonucu hafızaya alındı.")))
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

  output$scorecard_table <- renderTable({
    score <- model_scorecard(config$db_path)
    score |>
      dplyr::mutate(value = dplyr::case_when(
        metric == "Kapsanan maç" ~ as.character(as.integer(value)),
        is.na(value) ~ "Veri bekleniyor",
        TRUE ~ format(round(value, 3), nsmall = 3)
      )) |>
      dplyr::rename(Metrik = metric, Değer = value)
  }, width = "100%")
}
