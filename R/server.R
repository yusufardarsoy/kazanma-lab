app_server <- function(input, output, session, config) {
  authenticated <- reactiveVal(FALSE)
  login_message <- reactiveVal(NULL)
  failed_logins <- reactiveVal(0L)
  locked_until <- reactiveVal(as.POSIXct(NA))
  default_fixture_id <- unname(super_lig_fixture_choices(db_path = config$db_path)[[1]])
  build_selected_prediction <- function(fixture_id = default_fixture_id) {
    data <- super_lig_match_data(fixture_id) |>
      apply_provider_context(config$db_path) |>
      apply_postmatch_learning(config$db_path)
    data$actual_result <- match_actual_result(fixture_id, config$db_path)
    build_prediction(data)
  }
  prediction_value <- reactiveVal(build_selected_prediction())
  import_message <- reactiveVal(NULL)
  live_snapshot <- reactiveVal(NULL)
  data_version <- reactiveVal(1L)

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
      odds = odds_ui(),
      lineups = lineups_ui(),
      agent_tactics = agent_tactics_ui(),
      styles = styles_ui(),
      teams = teams_ui(),
      players = players_ui(),
      memory = memory_ui(),
      knowledge = knowledge_ui(),
      overview_ui()
    )
  })

  observeEvent(input$team_filter, {
    req(authenticated())
    choices <- super_lig_fixture_choices(input$team_filter %||% "", db_path = config$db_path)
    selected <- if (length(choices)) unname(choices[[1]]) else character()
    updateSelectizeInput(session, "fixture_id", choices = choices, selected = selected, server = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$fixture_id, {
    req(authenticated(), input$fixture_id)
    prediction_value(build_selected_prediction(input$fixture_id))
  }, ignoreInit = TRUE)

  observeEvent(input$run_analysis, {
    req(authenticated(), input$fixture_id)
    prediction <- build_selected_prediction(input$fixture_id)
    prediction_value(prediction)
    if (!isTRUE(prediction$fixture$scheduled[[1]])) {
      showNotification("Bu eşleşmenin resmi günü/saatı henüz açıklanmadı. Önizleme açık; tahmin dondurulmadı.", type = "warning", duration = 7)
      return()
    }
    record_analysis(prediction, config$db_path)
    data_version(data_version() + 1L)
    showNotification("Tahmin zaman damgasıyla donduruldu ve karşılaştırma hafızasına kaydedildi.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$sync_live, {
    req(authenticated())
    showNotification("Süper Lig canlı skorları, oranları ve tamamlanan maçlar eşitleniyor…", duration = 3)
    tryCatch({
      snapshot <- auto_sync_all_sources(config, force_public = TRUE, force_odds = FALSE)
      live_snapshot(snapshot)
      data_version(data_version() + 1L)
      choices <- super_lig_fixture_choices(input$team_filter %||% "", db_path = config$db_path)
      updateSelectizeInput(session, "fixture_id", choices = choices, selected = input$fixture_id %||% default_fixture_id, server = TRUE)
      updateSelectInput(session, "result_fixture_id", choices = super_lig_fixture_choices(scheduled_only = TRUE, db_path = config$db_path))
      prediction_value(build_selected_prediction(input$fixture_id %||% default_fixture_id))
      showNotification(paste0("Veri eşitleme başarılı: ", snapshot$results, " maç sonucu ve ", snapshot$odds_rows, " oran satırı işlendi."), type = "message")
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
        identical(p$data_mode, "provider_schedule") ~ "API-Football resmî API · TFF fikstür eşlemesi",
        identical(p$data_mode, "public_schedule") ~ "Football-Data.co.uk · TFF fikstürüyle eşleme",
        p$is_demo ~ "Sentetik veri",
        TRUE ~ "Canlı veri"
      ))
    )
  })

  output$match_header <- renderUI({
    p <- prediction()
    kickoff_label <- if (isTRUE(p$fixture$scheduled[[1]])) {
      format(p$fixture$kickoff, "%d %B %Y %H:%M")
    } else {
      paste0(p$fixture$round, ". hafta · tarih bekleniyor")
    }
    venue_label <- if (!is.na(p$fixture$venue[[1]]) && nzchar(p$fixture$venue[[1]])) p$fixture$venue[[1]] else "Stadyum programla açıklanacak"
    
    score_box <- if (!is.null(p$actual_result)) {
      res <- p$actual_result
      is_exact <- (p$likely_score$home == res$home_goals && p$likely_score$away == res$away_goals)
      actual_outcome <- if (res$home_goals > res$away_goals) "home" else if (res$home_goals < res$away_goals) "away" else "draw"
      pred_outcome <- names(which.max(p$outcomes))
      is_outcome_correct <- (pred_outcome == actual_outcome)
      badge_class <- if (is_exact) "badge-exact" else if (is_outcome_correct) "badge-correct" else "badge-incorrect"
      badge_text <- if (is_exact) "Tam Skor İsabeti!" else if (is_outcome_correct) "1X2 Doğru Tahmin" else "Farklı Sonuçlandı"

      xg_text <- if (!is.na(res$home_xg) && !is.na(res$away_xg)) {
        paste0("xG: ", round(res$home_xg, 2), " – ", round(res$away_xg, 2))
      } else {
        "Resmi Maç Sonu"
      }

      div(
        class = "score-call-container",
        div(
          class = "score-call",
          span("Model Tahmini (En olası)"),
          strong(paste0(p$likely_score$home, "–", p$likely_score$away)),
          tags$small(paste("Olasılık:", scales::percent(p$likely_score$probability, accuracy = .1)))
        ),
        div(
          class = "score-call score-call-actual",
          div(class = "score-header-flex", span("Gerçekleşen Skor (MS)"), tags$span(class = paste("result-badge", badge_class), badge_text)),
          strong(class = "actual-score-text", paste0(res$home_goals, " – ", res$away_goals)),
          tags$small(class = "actual-meta", xg_text)
        )
      )
    } else {
      div(
        class = "score-call",
        span("En olası skor"),
        strong(paste0(p$likely_score$home, "–", p$likely_score$away)),
        tags$small(scales::percent(p$likely_score$probability, accuracy = .1))
      )
    }

    div(
      class = "match-header",
      div(
        div(class = "eyebrow", paste(p$fixture$competition, "·", kickoff_label)),
        h1(paste(p$home$team, "—", p$away$team)),
        p(paste(venue_label, "· Model güveni", scales::percent(p$confidence, accuracy = 1)))
      ),
      score_box
    )
  })

  output$hero_cards <- renderUI({
    p <- prediction()
    div(
      class = "metric-strip",
      metric_card("1. En Olası Skor", paste0(p$likely_score$home, "–", p$likely_score$away), paste("Olasılık", scales::percent(p$likely_score$probability, accuracy = .1)), "gold"),
      metric_card("En Olası İY / MS", p$htft$most_likely_htft$code, paste(p$htft$most_likely_htft$label, "·", scales::percent(p$htft$most_likely_htft$probability, accuracy = .1)), "teal"),
      metric_card("İlk Yarı Skoru", p$htft$most_likely_ht_score$score, paste("Olasılık", scales::percent(p$htft$most_likely_ht_score$probability, accuracy = .1)), "violet"),
      metric_card("Beklenen Gol (xG)", paste0(round(p$expected_goals[["home"]], 2), " – ", round(p$expected_goals[["away"]], 2)), paste("Model Güveni", scales::percent(p$confidence, accuracy = 1)), "neutral")
    )
  })

  output$top_scores_plot <- renderPlot(plot_top_scores(prediction(), n = 8L), bg = "transparent", res = 110)

  output$top_scores_table <- renderTable({
    p <- prediction()
    req(p$top_scores)
    p$top_scores |>
      dplyr::slice_head(n = 6L) |>
      dplyr::transmute(
        Sıra = paste0("#", rank),
        Skor = score,
        `Sonuç Türü` = outcome,
        Olasılık = scales::percent(probability, accuracy = .1),
        `Adil Oran` = format(round(fair_odds, 2), nsmall = 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, align = "c")

  output$htft_plot <- renderPlot(plot_htft_probabilities(prediction()), bg = "transparent", res = 110)

  output$htft_table <- renderTable({
    p <- prediction()
    req(p$htft$htft_table)
    p$htft$htft_table |>
      dplyr::transmute(
        `İY/MS` = code,
        Senaryo = label,
        Olasılık = scales::percent(probability, accuracy = .1),
        `Adil Oran` = format(round(fair_odds, 2), nsmall = 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, align = "c")

  output$outcome_plot <- renderPlot(plot_outcomes(prediction()), bg = "transparent", res = 110)
  output$score_plot <- renderPlot(plot_score_matrix(prediction()), bg = "transparent", res = 110)
  output$style_plot <- renderPlot(plot_styles(prediction()), bg = "transparent", res = 110)
  output$scorer_plot <- renderPlot(plot_player_probability(prediction(), "scorer"), bg = "transparent", res = 110)
  output$card_plot <- renderPlot(plot_player_probability(prediction(), "card"), bg = "transparent", res = 110)

  odds_snapshot <- reactive(stored_odds_for_prediction(prediction(), config$db_path))
  odds_comparison <- reactive(compare_odds(prediction(), odds_snapshot()))

  output$overview_odds_teaser <- renderUI({
    comparison <- odds_comparison()
    if (nrow(comparison) == 0) return(NULL)
    top <- top_odds_options(comparison, 3L)
    div(
      class = "panel odds-teaser",
      div(
        div(class = "panel-kicker", paste("ORAN RADARI ·", paste(unique(comparison$source), collapse = " + "))),
        h3("Modelin piyasa fiyatından ayrıldığı seçenekler"),
        p("Kaynak zaman damgalı piyasa görüntüsüdür; model farkı kesin kazanç anlamına gelmez.")
      ),
      div(
        class = "odds-option-grid",
        lapply(seq_len(nrow(top)), function(i) {
          row <- top[i, ]
          div(
            class = "odds-option",
            span(paste(row$market, "·", row$selection)),
            strong(format(row$odds, nsmall = 2)),
            tags$small(paste("Model", scales::percent(row$model_probability, accuracy = .1), "·", row$signal))
          )
        })
      ),
      actionLink("open_odds", "Tüm oran karşılaştırmasını aç →", class = "odds-link")
    )
  })

  observeEvent(input$open_odds, {
    updateRadioButtons(session, "section", selected = "odds")
  }, ignoreInit = TRUE)

  output$odds_empty_state <- renderUI({
    if (nrow(odds_comparison()) > 0) return(NULL)
    div(
      class = "panel caveat-panel",
      strong("Bu maç için oran görüntüsü yok."),
      " Tahmin motoru çalışır; oran karşılaştırması yalnızca tarih ve kaynak bilgisi olan bir görüntü eklendiğinde açılır."
    )
  })

  output$odds_summary_cards <- renderUI({
    summary <- odds_snapshot_summary(odds_comparison())
    if (is.null(summary)) return(NULL)
    div(
      class = "metric-strip",
      metric_card("Model favorisi", summary$model_favorite, "1X2 model olasılığı", "gold"),
      metric_card("Piyasa favorisi", summary$market_favorite, "Marj temizlenmeden en düşük oran", "teal"),
      metric_card("1X2 bahis marjı", scales::percent(summary$bookmaker_margin, accuracy = .1), "Ham oranlar toplamı − %100", "violet"),
      metric_card("Veri uyarısı", paste(summary$stale_count, "bayat satır"), "168 saatten eski fiyatlar değerlendirme dışı", "neutral")
    )
  })

  output$odds_plot <- renderPlot({
    comparison <- odds_comparison()
    validate(need(nrow(comparison) > 0, "Bu maç için oran verisi yok."))
    plot_odds_comparison(comparison)
  }, bg = "transparent", res = 110)

  format_odds_table <- function(data) {
    data |>
      dplyr::transmute(
        Market = market,
        Seçenek = selection,
        Oran = format(round(odds, 2), nsmall = 2),
        Model = scales::percent(model_probability, accuracy = .1),
        `Piyasa / başabaş` = scales::percent(market_probability, accuracy = .1),
        `Fark` = scales::percent(edge, accuracy = .1),
        `Beklenen değer` = scales::percent(expected_return, accuracy = .1),
        Sinyal = signal,
        Risk = risk
      )
  }

  output$odds_top_table <- renderTable({
    comparison <- odds_comparison()
    validate(need(nrow(comparison) > 0, "Bu maç için oran verisi yok."))
    format_odds_table(top_odds_options(comparison, 8L))
  }, striped = FALSE, bordered = FALSE, hover = TRUE, width = "100%", align = "l")

  output$odds_all_table <- renderTable({
    comparison <- odds_comparison()
    validate(need(nrow(comparison) > 0, "Bu maç için oran verisi yok."))
    comparison |>
      dplyr::arrange(factor(market_id, levels = unique(market_id)), dplyr::desc(decision_score)) |>
      format_odds_table()
  }, striped = FALSE, bordered = FALSE, hover = TRUE, width = "100%", align = "l")

  output$odds_quality_note <- renderUI({
    comparison <- odds_comparison()
    if (nrow(comparison) == 0) return(NULL)
    result_margin <- unique(comparison$market_margin[comparison$market_id == "result"])[[1]]
    div(
      class = "panel caveat-panel odds-quality",
      strong("Kalite ve güvenlik notu:"),
      paste0(" 1X2 ham olasılıklarının toplamı %", round((1 + result_margin) * 100, 1),
             "; tabloda karşılaştırma için marj temizlenmiştir. Çifte şans ve golcü seçenekleri birbiriyle örtüştüğü için normalize edilmez, yalnızca başabaş olasılığı kullanılır. "),
      "Korner ve kart bahisleri için kalibre edilmiş olay modeli olmadığı için bu marketlere sahte olasılık eklenmedi. Oranlar sağlayıcının son yayımladığı görüntüdür; bu ekran finansal tavsiye değildir."
    )
  })

  output$tactical_notes <- renderUI({
    div(class = "insight-list", lapply(prediction()$tactical_notes, function(note) div(class = "insight-item", span(class = "insight-dot"), p(note))))
  })

  output$model_note <- renderUI({
    p <- prediction()
    div(
      class = "model-note",
      div(strong("Gol modeli"), span("Poisson + takım gücü + taktik eşleşme")),
      div(strong("11 modeli"), span(if (p$official_lineup_teams == 2L) "İki takımın resmî ilk 11'i işlendi" else if (p$lineup_role_based) "Güncel kadro yok: rol bazlı öncül" else "Başlama × süre × uygunluk")),
      div(strong("Oyuncu modeli"), span(if (p$lineup_role_based) "Oyuncu adı değil, rol olasılığı" else "Dakika ağırlıklı olay oranı")),
      div(strong("Öğrenme hafızası"), span(paste(p$learning_matches, "maç sonucu"))),
      if (!is.null(live_snapshot())) div(strong("Otomatik veri"), span("Son eşitleme tamamlandı ve hafızaya işlendi"))
    )
  })

  output$home_lineup <- renderUI(lineup_team_ui(prediction()$home, prediction()$home_xi))
  output$away_lineup <- renderUI(lineup_team_ui(prediction()$away, prediction()$away_xi))

  output$availability_status <- renderUI({
    p <- prediction()
    if (is.null(p$provider_status)) {
      return(div(class = "source-note", strong("API verisi bekleniyor"), span("Ücretsiz API anahtarı eklenince eksikler ve resmî ilk 11 otomatik görünecek.")))
    }
    status <- if (p$official_lineup_teams == 2L) "İki ilk 11 de resmî" else "Resmî ilk 11 henüz tamamlanmadı"
    div(class = "source-note", strong(status), span(paste("Sağlayıcı durumu:", p$provider_status$status_long[[1]], "· son eşitleme", p$provider_status$last_synced_at[[1]])))
  })

  output$availability_table <- renderTable({
    absences <- prediction()$availability
    if (nrow(absences) == 0) return(data.frame(Bilgi = "Sağlayıcının bu maç için bildirdiği eksik kaydı yok veya veri henüz yayınlanmadı."))
    absences |>
      dplyr::transmute(Takım = team, Oyuncu = player, Durum = dplyr::coalesce(absence_type, "Belirtilmedi"), Neden = dplyr::coalesce(reason, "Belirtilmedi"))
  }, width = "100%", striped = FALSE, bordered = FALSE)

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
      data_version(data_version() + 1L)
      choices <- super_lig_fixture_choices(input$team_filter %||% "", db_path = config$db_path)
      updateSelectizeInput(session, "fixture_id", choices = choices, selected = input$fixture_id %||% default_fixture_id, server = TRUE)
      updateSelectInput(session, "result_fixture_id", choices = super_lig_fixture_choices(scheduled_only = TRUE, db_path = config$db_path))
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
      data_version(data_version() + 1L)
      choices <- super_lig_fixture_choices(input$team_filter %||% "", db_path = config$db_path)
      updateSelectizeInput(session, "fixture_id", choices = choices, selected = input$fixture_id %||% default_fixture_id, server = TRUE)
      updateSelectInput(session, "result_fixture_id", choices = super_lig_fixture_choices(scheduled_only = TRUE, db_path = config$db_path))
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
    data_version()
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
    data_version()
    comparison <- prediction_result_history(config$db_path)
    if (nrow(comparison) == 0) return(data.frame(Bilgi = "Henüz sonuçtan önce kaydedilmiş tahmin–sonuç eşleşmesi yok."))
    comparison
  }, width = "100%")

  output$scorecard_table <- renderTable({
    data_version()
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
    data_version()
    div(class = "source-note", strong("Nasıl okunur?"), span(scorecard_interpretation(config$db_path)))
  })

  output$automation_health_table <- renderTable({
    data_version()
    health <- automation_health(config$db_path)
    counts <- health$counts[1, ]
    data.frame(
      Gösterge = c("API-Football fikstürü", "Oran bulunan maç", "Saklanan oran satırı", "İlk 11 kaydı olan maç", "Eksik oyuncu kaydı", "Kaydedilmiş sonuç", "Kaynaklı sonuç", "Ayrıntılı maç-sonu paket"),
      Değer = as.integer(c(counts$fixtures, counts$odds_matches, counts$odds_rows, counts$lineup_matches, counts$absences, counts$results, counts$sourced_results, counts$detailed_matches)),
      check.names = FALSE
    )
  }, width = "100%")

  output$automation_note <- renderUI({
    data_version()
    health <- automation_health(config$db_path)
    if (nrow(health$last_public_run) == 0) {
      return(div(class = "source-note", strong("Henüz otomatik eşitleme yok."), span("Şimdi veri eşitle düğmesini kullanabilir veya Windows görevini kurabilirsin; temel akış API anahtarı istemez.")))
    }
    run <- health$last_public_run[1, ]
    div(class = "source-note", strong(if (run$status %in% c("ok", "cached")) "Son görev kullanılabilir" else "Son görev hata verdi"), span(paste(run$source, "·", run$finished_at, "·", run$message)))
  })

  team_learning_data <- reactive({
    data_version()
    prediction_value()
    team_learning_summary(config$db_path)
  })

  finished_matches_data <- reactive({
    data_version()
    prediction_value()
    finished_matches_detailed(config$db_path)
  })

  output$memory_hero_cards <- renderUI({
    matches <- finished_matches_data()
    summary <- team_learning_data()
    n_matches <- nrow(matches)
    avg_xg_val <- if (n_matches > 0 && !all(is.na(summary$avg_xg_for))) {
      round(mean(summary$avg_xg_for, na.rm = TRUE), 2)
    } else {
      "—"
    }
    score <- model_scorecard(config$db_path)
    accuracy_val <- score$value[score$metric == "1X2 isabeti"][[1]]
    accuracy_text <- if (!is.na(accuracy_val)) scales::percent(accuracy_val, accuracy = .1) else "Örneklem bekleniyor"

    top_evolved <- summary |> dplyr::filter(matches_played > 0)
    top_note <- if (nrow(top_evolved) > 0) {
      best <- top_evolved |> dplyr::slice_max(attack_delta, n = 1, with_ties = FALSE)
      paste0(best$short, " (", ifelse(best$attack_delta >= 0, paste0("+", best$attack_delta), as.character(best$attack_delta)), " hücum)")
    } else {
      "—"
    }

    div(
      class = "metric-strip",
      metric_card("Tamamlanan maç", paste(n_matches, "maç"), "Veritabanında kayıtlı sonuç", "gold"),
      metric_card("Ortalama xG / maç", as.character(avg_xg_val), "Gerçekleşen gol beklentisi", "teal"),
      metric_card("1X2 tahmin isabeti", accuracy_text, "Maç önü dondurulmuş tahminler", "violet"),
      metric_card("En çok gelişen", top_note, "Biten maç performansı sonrası", "neutral")
    )
  })

  output$learning_evolution_plot <- renderPlot({
    summary <- team_learning_data()
    validate(need(nrow(summary) > 0, "Öğrenme verisi bulunamadı."))
    plot_team_learning_evolution(summary)
  }, bg = "transparent", res = 110)

  output$team_learning_table <- renderTable({
    summary <- team_learning_data()
    validate(need(nrow(summary) > 0, "Öğrenme verisi bulunamadı."))
    summary |>
      dplyr::transmute(
        Takım = team,
        `Oynanan Maç` = as.integer(matches_played),
        `Hücum (Başlangıç → Güncel)` = paste0(base_attack, " → ", current_attack, " (", ifelse(attack_delta >= 0, paste0("+", attack_delta), as.character(attack_delta)), ")"),
        `Savunma (Başlangıç → Güncel)` = paste0(base_defence, " → ", current_defence, " (", ifelse(defence_delta >= 0, paste0("+", defence_delta), as.character(defence_delta)), ")"),
        `Disiplin` = paste0(base_discipline, " → ", current_discipline),
        `Maç Başı xG` = ifelse(is.na(avg_xg_for), "—", format(avg_xg_for, nsmall = 2)),
        `Yenilen Gol / Maç` = ifelse(is.na(avg_goals_against), "—", format(avg_goals_against, nsmall = 2)),
        `Modele Etki Payı` = scales::percent(learning_weight, accuracy = 1)
      )
  }, striped = FALSE, bordered = FALSE, hover = TRUE, width = "100%", align = "l")

  output$finished_matches_table <- renderTable({
    matches <- finished_matches_data()
    if (nrow(matches) == 0) return(data.frame(Bilgi = "Henüz kaydedilmiş biten maç sonucu yok."))
    matches
  }, striped = FALSE, bordered = FALSE, hover = TRUE, width = "100%", align = "l")

  # --- Live Scoreboard & In-Match Score Tracking ---
  live_scores_reactive <- reactive({
    data_version()
    get_live_scores_db(config$db_path)
  })

  output$live_matches_banner <- renderUI({
    live_df <- live_scores_reactive()
    if (nrow(live_df) == 0) {
      return(div(
        class = "source-note",
        style = "margin-bottom: 16px; background: rgba(39, 51, 46, 0.4); border-left: 3px solid #63B4A5;",
        strong("ℹ️ Canlı Maç Durumu: "),
        span("Şu anda canlı oynanmakta olan maç yok veya maçlar henüz başlamadı. '🔴 Canlı Skorları Tara & Eşitle' butonuna basarak maç anındaki anlık skorları çekebilirsiniz.")
      ))
    }
    
    cards <- lapply(seq_len(nrow(live_df)), function(i) {
      r <- live_df[i, ]
      div(
        class = "panel",
        style = "margin-bottom: 12px; background: rgba(16, 24, 21, 0.85); border: 1px solid rgba(224, 106, 106, 0.4);",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          div(
            span(class = "status-badge live", style = "background: #E06A6A; color: white;", paste0("🔴 ", r$minute %||% "CANLI")),
            strong(style = "font-size: 1.15rem; margin-left: 10px; color: #F5F8F6;", paste0(r$home_team, " ", r$home_goals, " – ", r$away_goals, " ", r$away_team))
          ),
          span(style = "color: #94A3B8; font-size: 0.85rem;", paste0("Son Güncelleme: ", format(Sys.time(), "%H:%M:%S")))
        )
      )
    })
    tagList(cards)
  })

  observeEvent(input$sync_live_scores_btn, {
    showNotification("Canlı maç skorları ESPN & Açık servislerden taranıyor…", type = "message", duration = 3)
    res <- sync_public_scoreboard_scores(config)
    data_version(data_version() + 1L)
    showNotification(res$message, type = "default", duration = 4)
  })

  # --- Donut Charts & Prediction Accuracy Analytics ---
  exact_score_stats_reactive <- reactive({
    data_version()
    prediction_value()
    score_prediction_accuracy_stats(config$db_path)
  })

  output$exact_score_donut_plot <- renderPlot({
    stats <- exact_score_stats_reactive()
    plot_exact_score_donut(stats)
  }, bg = "transparent", res = 110)

  output$exact_score_stats_pills <- renderUI({
    stats <- exact_score_stats_reactive()
    div(
      class = "metric-strip",
      style = "margin-top: 14px;",
      metric_card("Tam İsabet", paste0("%", round(stats$exact_rate * 100, 1)), paste0(stats$exact_hits, " / ", stats$total, " maç"), "teal"),
      metric_card("Top-3 İsabet", paste0("%", round(stats$top3_rate * 100, 1)), paste0(stats$exact_hits + stats$top3_hits, " / ", stats$total, " maç"), "gold"),
      metric_card("Farklı Skor", paste0(stats$misses, " maç"), "Model dışı skor", "neutral")
    )
  })

  htft_stats_reactive <- reactive({
    data_version()
    prediction_value()
    htft_prediction_accuracy_stats(config$db_path)
  })

  output$htft_donut_plot <- renderPlot({
    stats <- htft_stats_reactive()
    plot_htft_donut(stats)
  }, bg = "transparent", res = 110)

  output$htft_stats_pills <- renderUI({
    stats <- htft_stats_reactive()
    div(
      class = "metric-strip",
      style = "margin-top: 14px;",
      metric_card("İY/MS Tam İsabet", paste0("%", round(stats$htft_rate * 100, 1)), paste0(stats$htft_hits, " / ", stats$total, " maç"), "gold"),
      metric_card("1X2 İsabeti", paste0("%", round(stats$ft_rate * 100, 1)), paste0(stats$htft_hits + stats$ft_only_hits, " / ", stats$total, " maç"), "teal"),
      metric_card("Tutmayan", paste0(stats$misses, " maç"), "Farklı kombinasyon", "neutral")
    )
  })

  # --- AI Tactical Intelligence & 11 vs 11 Heatmap Engine ---
  ai_scout_result <- reactiveVal(NULL)
  ai_scout_loading <- reactiveVal(FALSE)

  render_zone_bars_widget <- function(h) {
    div(
      class = "zone-bars-container",
      div(class = "zone-bar-row", span("3. Bölge (Hücum)"), div(class = "zone-progress", div(class = "zone-bar-fill gold", style = paste0("width: ", h$attacking_third_pct %||% 0, "%;"))), span(paste0("%", h$attacking_third_pct %||% 0))),
      div(class = "zone-bar-row", span("Orta Alan"), div(class = "zone-progress", div(class = "zone-bar-fill teal", style = paste0("width: ", h$middle_third_pct %||% 0, "%;"))), span(paste0("%", h$middle_third_pct %||% 0))),
      div(class = "zone-bar-row", span("1. Bölge (Savunma)"), div(class = "zone-progress", div(class = "zone-bar-fill violet", style = paste0("width: ", h$defensive_third_pct %||% 0, "%;"))), span(paste0("%", h$defensive_third_pct %||% 0))),
      div(class = "zone-bar-row", span("Ceza Sahası Girişi"), div(class = "zone-progress", div(class = "zone-bar-fill red", style = paste0("width: ", h$box_penetration_pct %||% 0, "%;"))), span(paste0("%", h$box_penetration_pct %||% 0)))
    )
  }

  output$positional_matchups_grid <- renderUI({
    p <- prediction()
    h_xi <- if (!is.null(p$home_xi) && nrow(p$home_xi) > 0) p$home_xi else tibble::tibble()
    a_xi <- if (!is.null(p$away_xi) && nrow(p$away_xi) > 0) p$away_xi else tibble::tibble()
    
    if (nrow(h_xi) == 0 || nrow(a_xi) == 0) {
      return(div(class = "ai-scout-empty", p("İlk 11 kadro verisi hazırlanıyor…")))
    }
    
    sector_filter <- input$tactics_sector_filter %||% "all"
    n_pairs <- min(nrow(h_xi), nrow(a_xi))
    
    matchup_cards <- list()
    
    for (i in seq_len(n_pairs)) {
      hp <- h_xi[i, ]
      ap <- a_xi[i, ]
      
      pos <- hp$position
      if (!identical(sector_filter, "all") && !identical(pos, sector_filter)) {
        next
      }
      
      pos_label <- switch(
        pos,
        GK = "🧤 KALECİ EŞLEŞMESİ",
        DEF = "🛡️ DEFANS / BEK HATTI",
        MID = "⚙️ ORTA SAHA MERKEZİ",
        FWD = "⚡ HÜCUM & FORVET HATTI",
        "MEVKİ EŞLEŞMESİ"
      )
      
      h_side <- if (grepl("sağ|sag|right", tolower(hp$role))) "right" else if (grepl("sol|left", tolower(hp$role))) "left" else "center"
      a_side <- if (grepl("sağ|sag|right", tolower(ap$role))) "right" else if (grepl("sol|left", tolower(ap$role))) "left" else "center"
      
      h_map <- run_player_heatmap(
        player_name = hp$player,
        team_name = p$home$team,
        opponent_name = p$away$team,
        role = hp$role,
        side = h_side,
        is_home = TRUE,
        team_possession = p$home$possession %||% 50,
        team_pressing = p$home$pressing %||% 50,
        team_directness = p$home$directness %||% 50,
        team_width = p$home$width %||% 50,
        opp_possession = p$away$possession %||% 50,
        opp_pressing = p$away$pressing %||% 50,
        opp_defence = p$away$defence %||% 50,
        config = config
      )
      
      a_map <- run_player_heatmap(
        player_name = ap$player,
        team_name = p$away$team,
        opponent_name = p$home$team,
        role = ap$role,
        side = a_side,
        is_home = FALSE,
        team_possession = p$away$possession %||% 50,
        team_pressing = p$away$pressing %||% 50,
        team_directness = p$away$directness %||% 50,
        team_width = p$away$width %||% 50,
        opp_possession = p$home$possession %||% 50,
        opp_pressing = p$home$pressing %||% 50,
        opp_defence = p$home$defence %||% 50,
        config = config
      )
      
      h_att <- h_map$attacking_third_pct %||% 0
      a_att <- a_map$attacking_third_pct %||% 0
      adv_text <- if (h_att > a_att) {
        paste0(p$home$team, " +%", round(h_att - a_att, 1), " Baskı")
      } else {
        paste0(p$away$team, " +%", round(a_att - h_att, 1), " Baskı")
      }
      
      card <- div(
        class = "panel matchup-position-card",
        style = "margin-bottom: 22px; border: 1px solid rgba(148, 163, 184, 0.2);",
        div(
          class = "matchup-card-header",
          style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid rgba(148, 163, 184, 0.15); padding-bottom: 10px; margin-bottom: 14px;",
          div(
            span(class = "panel-kicker", pos_label),
            h3(style = "margin: 4px 0 0; font-size: 1.15rem;", paste0(hp$player, " (", p$home$team, ") vs ", ap$player, " (", p$away$team, ")"))
          ),
          status_badge(adv_text, "live")
        ),
        div(
          class = "two-column",
          div(
            class = "player-subcard",
            div(strong(hp$player), span(paste0(" (", hp$role, " · ", hp$position, ")")), style = "margin-bottom: 8px; color: #f8fafc; font-size: 0.95rem;"),
            tags$img(src = h_map$image_path, style = "width: 100%; border-radius: 8px; border: 1px solid rgba(148, 163, 184, 0.15);"),
            render_zone_bars_widget(h_map)
          ),
          div(
            class = "player-subcard",
            div(strong(ap$player), span(paste0(" (", ap$role, " · ", ap$position, ")")), style = "margin-bottom: 8px; color: #f8fafc; font-size: 0.95rem;"),
            tags$img(src = a_map$image_path, style = "width: 100%; border-radius: 8px; border: 1px solid rgba(148, 163, 184, 0.15);"),
            render_zone_bars_widget(a_map)
          )
        )
      )
      matchup_cards[[length(matchup_cards) + 1L]] <- card
    }
    
    if (length(matchup_cards) == 0) {
      return(div(class = "ai-scout-empty", p("Seçilen mevkide oyuncu eşleşmesi bulunamadı.")))
    }
    
    tagList(matchup_cards)
  })

  observeEvent(input$generate_ai_scout, {
    p <- prediction()
    showNotification("NVIDIA NIM Llama 3.2 Taktik Ajanı analiz üretiyor…", type = "message", duration = 4)
    ai_scout_loading(TRUE)

    h_lead <- if (!is.null(p$home_xi) && nrow(p$home_xi) > 0) p$home_xi[nrow(p$home_xi), ] else list(player = "Hücum Lideri", role = "Forvet")
    a_lead <- if (!is.null(p$away_xi) && nrow(p$away_xi) > 1) p$away_xi[2, ] else list(player = "Savunma Lideri", role = "Stoper")
    
    h_side <- if (grepl("sağ|sag|right", tolower(h_lead$role))) "right" else if (grepl("sol|left", tolower(h_lead$role))) "left" else "center"
    a_side <- if (grepl("sağ|sag|right", tolower(a_lead$role))) "right" else if (grepl("sol|left", tolower(a_lead$role))) "left" else "center"

    h_metrics <- run_player_heatmap(
      player_name = h_lead$player,
      team_name = p$home$team,
      opponent_name = p$away$team,
      role = h_lead$role,
      side = h_side,
      is_home = TRUE,
      team_possession = p$home$possession %||% 50,
      team_pressing = p$home$pressing %||% 50,
      team_directness = p$home$directness %||% 50,
      team_width = p$home$width %||% 50,
      opp_possession = p$away$possession %||% 50,
      opp_pressing = p$away$pressing %||% 50,
      opp_defence = p$away$defence %||% 50,
      config = config
    )
    a_metrics <- run_player_heatmap(
      player_name = a_lead$player,
      team_name = p$away$team,
      opponent_name = p$home$team,
      role = a_lead$role,
      side = a_side,
      is_home = FALSE,
      team_possession = p$away$possession %||% 50,
      team_pressing = p$away$pressing %||% 50,
      team_directness = p$away$directness %||% 50,
      team_width = p$away$width %||% 50,
      opp_possession = p$home$possession %||% 50,
      opp_pressing = p$home$pressing %||% 50,
      opp_defence = p$home$defence %||% 50,
      config = config
    )
    match_name <- paste(p$home$team, "vs", p$away$team)

    rep <- run_nvidia_ai_scout(
      match_name = match_name,
      home_team = p$home$team,
      away_team = p$away$team,
      home_player = h_lead$player,
      home_role = h_lead$role,
      away_player = a_lead$player,
      away_role = a_lead$role,
      home_metrics = h_metrics,
      away_metrics = a_metrics,
      config = config
    )
    ai_scout_loading(FALSE)
    ai_scout_result(rep)
  }, ignoreInit = TRUE)

  output$nvidia_ai_scout_report_view <- renderUI({
    rep <- ai_scout_result()
    if (is.null(rep)) {
      latest <- get_latest_tactical_scout_report_db(config$db_path)
      if (!is.null(latest)) {
        rep <- list(
          status = "success",
          model = latest$model_name,
          duration_seconds = latest$duration_seconds,
          report_text = latest$report_text,
          home_player = latest$home_player,
          away_player = latest$away_player
        )
      } else {
        return(div(
          class = "ai-scout-empty",
          p("Henüz bir taktik raporu üretilmedi. '⚡ NVIDIA Llama 3.2 ile AI Taktik Raporu Üret' butonuna tıklayın.")
        ))
      }
    }

    if (!identical(rep$status, "success")) {
      return(div(class = "import-message error", paste("Hata:", rep$error_message %||% "NVIDIA NIM API yanıt vermedi.")))
    }

    tagList(
      div(
        class = "ai-report-meta",
        status_badge(paste0("Model: ", rep$model), "live"),
        status_badge(paste0("İşlem Süresi: ", rep$duration_seconds, "s"), "neutral")
      ),
      div(
        class = "ai-report-content",
        HTML(commonmark::markdown_html(rep$report_text))
      )
    )
  })
}
