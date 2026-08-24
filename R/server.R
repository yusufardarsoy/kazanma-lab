app_server <- function(input, output, session, config) {
  authenticated <- reactiveVal(FALSE)
  login_message <- reactiveVal(NULL)
  failed_logins <- reactiveVal(0L)
  locked_until <- reactiveVal(as.POSIXct(NA))
  default_fixture_id <- unname(super_lig_fixture_choices()[[1]])
  build_selected_prediction <- function(fixture_id = default_fixture_id) {
    data <- super_lig_match_data(fixture_id) |>
      apply_provider_context(config$db_path) |>
      apply_postmatch_learning(config$db_path)
    build_prediction(data)
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
      odds = odds_ui(),
      lineups = lineups_ui(),
      styles = styles_ui(),
      teams = teams_ui(),
      players = players_ui(),
      memory = memory_ui(),
      overview_ui()
    )
  })

  observeEvent(input$team_filter, {
    req(authenticated())
    choices <- super_lig_fixture_choices(input$team_filter %||% "")
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
    showNotification("Tahmin zaman damgasıyla donduruldu ve karşılaştırma hafızasına kaydedildi.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$sync_live, {
    req(authenticated(), api_football_enabled(config))
    showNotification("Süper Lig fikstürü, eksikler ve tamamlanan maçlar eşitleniyor…", duration = 3)
    tryCatch({
      snapshot <- auto_sync_league(config)
      live_snapshot(snapshot)
      prediction_value(build_selected_prediction(input$fixture_id %||% default_fixture_id))
      showNotification(paste0("Güncelleme tamamlandı: ", snapshot$fixtures_mapped, " maç eşleşti, ", snapshot$requests_used, " API isteği kullanıldı."), type = "message")
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
    div(
      class = "match-header",
      div(
        div(class = "eyebrow", paste(p$fixture$competition, "·", kickoff_label)),
        h1(paste(p$home$team, "—", p$away$team)),
        p(paste(venue_label, "· Model güveni", scales::percent(p$confidence, accuracy = 1)))
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

  odds_comparison <- reactive(compare_odds(prediction()))

  output$overview_odds_teaser <- renderUI({
    comparison <- odds_comparison()
    if (nrow(comparison) == 0) return(NULL)
    top <- top_odds_options(comparison, 3L)
    div(
      class = "panel odds-teaser",
      div(
        div(class = "panel-kicker", "ORAN RADARI · KULLANICI GÖRÜNTÜSÜ"),
        h3("Modelin piyasa fiyatından ayrıldığı seçenekler"),
        p("24 Ağustos oran görüntüsü; canlı değildir. Model farkı, kesin kazanç anlamına gelmez.")
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
      metric_card("Veri uyarısı", paste(summary$stale_count, "bayat satır"), "Petković oranı değerlendirme dışı", "neutral")
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
      "Korner ve kart bahisleri için kalibre edilmiş olay modeli olmadığı için pastedeki oranlara sahte olasılık eklenmedi. Bu ekran finansal tavsiye değildir."
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

  output$automation_health_table <- renderTable({
    health <- automation_health(config$db_path)
    counts <- health$counts[1, ]
    data.frame(
      Gösterge = c("Eşleşen fikstür", "İlk 11 kaydı olan maç", "Eksik oyuncu kaydı", "Kaydedilmiş sonuç", "Ayrıntılı maç-sonu paket"),
      Değer = as.integer(c(counts$fixtures, counts$lineup_matches, counts$absences, counts$results, counts$detailed_matches)),
      check.names = FALSE
    )
  }, width = "100%")

  output$automation_note <- renderUI({
    health <- automation_health(config$db_path)
    if (nrow(health$last_run) == 0) {
      return(div(class = "source-note", strong("Henüz otomatik eşitleme yok."), span(if (api_football_enabled(config)) "Şimdi eşitle düğmesini kullanabilir veya Windows görevini kurabilirsin." else "Önce ücretsiz API anahtarını .env dosyasına ekle.")))
    }
    run <- health$last_run[1, ]
    div(class = "source-note", strong(if (run$status == "ok") "Son görev başarılı" else "Son görev hata verdi"), span(paste(run$finished_at, "·", run$message)))
  })
}
