app_shell_ui <- function(config) {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$title("Kazanma Lab"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    uiOutput("root_ui")
  )
}

status_badge <- function(label, class = "neutral") {
  span(class = paste("status-badge", class), label)
}

metric_card <- function(label, value, detail, accent = "gold") {
  div(
    class = paste("metric-card", paste0("accent-", accent)),
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    div(class = "metric-detail", detail)
  )
}

dashboard_ui <- function(config) {
  div(
    class = "app-page",
    tags$header(
      class = "topbar",
      div(
        class = "brand-lockup",
        div(class = "brand-mark", "KL"),
        div(
          div(class = "eyebrow", "PRIVATE FOOTBALL INTELLIGENCE"),
          div(class = "brand-title", "Kazanma Lab")
        )
      ),
      div(
        class = "topbar-actions",
        status_badge(if (api_football_enabled(config)) "Süper Lig + canlı bağlantı" else "Süper Lig ön-modeli", if (api_football_enabled(config)) "live" else "demo"),
        actionButton("logout", "Çıkış", class = "btn-ghost btn-compact")
      )
    ),
    div(
      class = "app-grid",
      tags$aside(
        class = "sidebar",
        div(class = "sidebar-label", "ÇALIŞMA ALANI"),
        radioButtons(
          "section", NULL,
          choiceNames = c("Maç merkezi", "Oran radarı", "Muhtemel 11", "Stil savaşı", "Süper Lig DNA", "Oyuncu radarları", "Model hafızası"),
          choiceValues = c("overview", "odds", "lineups", "styles", "teams", "players", "memory"),
          selected = "overview"
        ),
        div(class = "sidebar-divider"),
        div(class = "sidebar-label", "FİKSTÜR ARAMA"),
        selectInput(
          "team_filter", "Takım",
          choices = c("Tüm takımlar" = "", stats::setNames(super_lig_teams()$team, super_lig_teams()$team))
        ),
        selectizeInput(
          "fixture_id", "Maç",
          choices = super_lig_fixture_choices(),
          options = list(placeholder = "Takım veya maç ara", maxOptions = 306)
        ),
        actionButton("run_analysis", "Tahmini kaydet", class = "btn-primary btn-block"),
        if (api_football_enabled(config)) {
          tagList(
            textInput("live_fixture_id", "Canlı fixture ID", placeholder = "örn. 1234567"),
            actionButton("sync_live", "Canlı özeti çek", class = "btn-secondary btn-block")
          )
        } else {
          div(
            class = "source-note",
            strong("Ücretsiz Süper Lig ön-modeli"),
            span("34 haftalık TFF fikstürü ve 2025-26 performansı kullanılıyor. Kocaelispor–Amed maçında güncel muhtemel 11; diğer maçlarda güncellenene kadar rol bazlı adaylar gösterilir.")
          )
        },
        div(class = "sidebar-divider"),
        uiOutput("freshness_ui")
      ),
      tags$main(
        class = "main-canvas",
        uiOutput("section_ui")
      )
    )
  )
}

overview_ui <- function() {
  tagList(
    uiOutput("match_header"),
    uiOutput("hero_cards"),
    uiOutput("overview_odds_teaser"),
    div(
      class = "two-column",
      div(class = "panel", plotOutput("outcome_plot", height = 340)),
      div(class = "panel", plotOutput("score_plot", height = 340))
    ),
    div(
      class = "two-column tactical-row",
      div(
        class = "panel",
        div(class = "panel-kicker", "MAÇ PLANI"),
        h3("Kritik taktik kırılmaları"),
        uiOutput("tactical_notes")
      ),
      div(
        class = "panel",
        div(class = "panel-kicker", "MODEL NOTU"),
        h3("Bu tahmini nasıl okumalı?") ,
        p("Olasılıklar kesin sonuç değildir. İlk 11, sakatlık, oran ve son dakika rol değişimleri geldikçe tahmin yeniden hesaplanır."),
        uiOutput("model_note")
      )
    )
  )
}

odds_ui <- function() {
  tagList(
    div(
      class = "section-heading",
      div(class = "eyebrow", "ODDS INTELLIGENCE"),
      h1("Oran radarı"),
      p("İddaa oranını model olasılığıyla karşılaştırır. Pozitif fark garanti veya kupon tavsiyesi değildir; veri ve model hatası her zaman mümkündür.")
    ),
    uiOutput("odds_empty_state"),
    uiOutput("odds_summary_cards"),
    div(
      class = "two-column",
      div(class = "panel", plotOutput("odds_plot", height = 430)),
      div(
        class = "panel",
        div(class = "panel-kicker", "EN MANTIKLI SEÇENEKLER"),
        h3("Fark ve risk birlikte sıralandı"),
        tableOutput("odds_top_table")
      )
    ),
    div(class = "panel odds-table-panel", h3("Tüm modellenen seçenekler"), tableOutput("odds_all_table")),
    uiOutput("odds_quality_note")
  )
}

lineups_ui <- function() {
  tagList(
    div(class = "section-heading", div(class = "eyebrow", "LINEUP ENGINE"), h1("Muhtemel ilk 11"), p("Başlama geçmişi, beklenen süre, form ve uygunluk sinyallerinin birleşimi.")),
    div(
      class = "two-column",
      div(class = "panel lineup-panel", uiOutput("home_lineup")),
      div(class = "panel lineup-panel", uiOutput("away_lineup"))
    ),
    div(class = "panel caveat-panel", strong("Önemli:"), " Resmi ilk 11 genellikle başlama vuruşuna yakın gelir. O ana kadar ekrandaki değerler tahmindir; resmi veri geldiğinde durum “onaylı” olarak değişmelidir.")
  )
}

styles_ui <- function() {
  tagList(
    div(class = "section-heading", div(class = "eyebrow", "MATCHUP DNA"), h1("Karşılıklı oyun stilleri"), p("Stil skorları mutlak kalite değil; takımın maçı nasıl oynamaya eğilimli olduğunu anlatır.")),
    div(class = "panel", plotOutput("style_plot", height = 470)),
    div(
      class = "three-column",
      uiOutput("style_cards")
    )
  )
}

teams_ui <- function() {
  tagList(
    div(
      class = "section-heading",
      div(class = "eyebrow", "LEAGUE INTELLIGENCE"),
      h1("18 takımın Süper Lig DNA'sı"),
      p("Teknik direktör, ana oyun fikri, güçlü yön, kırılganlık ve profil güveni. Taktik puanlar kamuya açık verilerden üretilmiş uzman öncülleridir; tracking verisi değildir.")
    ),
    div(class = "panel profile-table-panel", tableOutput("team_profile_table")),
    div(
      class = "panel caveat-panel",
      strong("Kaynak tarihi: 24 Ağustos 2026."),
      " Teknik direktör, transfer ve sakatlık bilgileri hızlı değişir. Motor her tahminde profil güvenini ayrıca düşürüp yükseltir."
    )
  )
}

players_ui <- function() {
  tagList(
    div(class = "section-heading", div(class = "eyebrow", "PLAYER MARKETS"), h1("Gol ve kart radarları"), p("Oyuncu oranları ilk 11 olasılığı ve beklenen dakika ile birlikte hesaplanır.")),
    div(
      class = "two-column",
      div(class = "panel", plotOutput("scorer_plot", height = 430)),
      div(class = "panel", plotOutput("card_plot", height = 430))
    ),
    div(class = "panel", h3("Oyuncu detayları"), tableOutput("player_table"))
  )
}

memory_ui <- function() {
  tagList(
    div(class = "section-heading", div(class = "eyebrow", "LEARNING LOOP"), h1("Model hafızası"), p("Her maç sonrası tahmin–gerçekleşen farkını kaydeder, kalibrasyonu izler ve yeni ağırlıklara veri sağlar.")),
    div(
      class = "two-column",
      div(
        class = "panel",
        h3("Gerçek sonucu gir"),
        p("Maç bittikten sonra skoru buradan kaydet. Sonuçtan sonra oluşturulan tahminler doğruluk hesabına alınmaz."),
        selectInput("result_fixture_id", "Süper Lig maçı", choices = super_lig_fixture_choices(scheduled_only = TRUE)),
        div(
          class = "result-input-row",
          numericInput("manual_home_goals", "Ev gol", value = 0, min = 0, max = 20, step = 1),
          numericInput("manual_away_goals", "Dep. gol", value = 0, min = 0, max = 20, step = 1)
        ),
        actionButton("save_manual_result", "Gerçek sonucu kaydet", class = "btn-primary"),
        div(class = "sidebar-divider"),
        h3("Toplu CSV yükle"),
        p("Aynı fixture ID tekrar gelirse kayıt güvenli biçimde güncellenir."),
        fileInput("postmatch_file", "Sonuç CSV", accept = c(".csv", "text/csv")),
        actionButton("import_postmatch", "Sonuçları hafızaya al", class = "btn-primary"),
        uiOutput("import_status")
      ),
      div(class = "panel", h3("Model sağlık kartı"), tableOutput("scorecard_table"), uiOutput("accuracy_note"))
    ),
    div(class = "panel", h3("Tahmin — gerçek sonuç karşılaştırması"), tableOutput("comparison_table")),
    div(class = "panel", h3("Son analizler"), tableOutput("history_table")),
    div(
      class = "panel roadmap-panel",
      h3("Öğrenme sırası"),
      div(class = "roadmap-step", span("01"), div(strong("Tahmin anı"), p("Olasılıklar ve veri sürümü dondurulur."))),
      div(class = "roadmap-step", span("02"), div(strong("Maç sonu"), p("Skor, xG, kart ve oyuncu olayları eklenir."))),
      div(class = "roadmap-step", span("03"), div(strong("Kalibrasyon"), p("Brier ve log loss ile model hatası ölçülür."))),
      div(class = "roadmap-step", span("04"), div(strong("Yeniden eğitim"), p("Takım/oyuncu ağırlıkları zaman çürümesiyle güncellenir.")))
    )
  )
}

lineup_team_ui <- function(team, xi) {
  position_label <- c(GK = "Kaleci", DEF = "Savunma", MID = "Orta saha", FWD = "Hücum")
  div(
    div(class = "lineup-head", div(h2(team$team), span(team$formation)), status_badge(team$lineup_status %||% "Tahmin", "demo")),
    div(
      class = "lineup-list",
      lapply(seq_len(nrow(xi)), function(i) {
        row <- xi[i, ]
        div(
          class = "lineup-player",
          div(class = "position-pill", position_label[[row$position]]),
          div(class = "player-copy", strong(row$player), span(row$role)),
          div(class = "probability-copy", scales::percent(row$start_probability, accuracy = 1))
        )
      })
    )
  )
}
