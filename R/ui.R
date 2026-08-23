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
    header(
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
        status_badge(if (api_football_enabled(config)) "Canlı veri anahtarı hazır" else "Demo veri", if (api_football_enabled(config)) "live" else "demo"),
        actionButton("logout", "Çıkış", class = "btn-ghost btn-compact")
      )
    ),
    div(
      class = "app-grid",
      aside(
        class = "sidebar",
        div(class = "sidebar-label", "ÇALIŞMA ALANI"),
        radioButtons(
          "section", NULL,
          choiceNames = c("Maç merkezi", "Muhtemel 11", "Stil savaşı", "Oyuncu radarları", "Model hafızası"),
          choiceValues = c("overview", "lineups", "styles", "players", "memory"),
          selected = "overview"
        ),
        div(class = "sidebar-divider"),
        div(class = "sidebar-label", "VERİ KAYNAĞI"),
        selectInput("fixture_id", NULL, choices = c("Boğaz FK — Anadolu 1907" = "DEMO-001")),
        actionButton("run_analysis", "Maçı yeniden analiz et", class = "btn-primary btn-block"),
        if (api_football_enabled(config)) {
          tagList(
            textInput("live_fixture_id", "Canlı fixture ID", placeholder = "örn. 1234567"),
            actionButton("sync_live", "Canlı özeti çek", class = "btn-secondary btn-block")
          )
        } else {
          div(
            class = "source-note",
            strong("Canlı bağlantı kapalı"),
            span("API anahtarı eklendiğinde fikstür, sakatlık ve resmi 11 verisi alınabilir.")
          )
        },
        div(class = "sidebar-divider"),
        uiOutput("freshness_ui")
      ),
      main(
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
        h3("Maç-sonu sonuç yükle"),
        p("CSV dosyanı şablona göre yükle. Aynı fixture ID tekrar gelirse kayıt güvenli biçimde güncellenir."),
        fileInput("postmatch_file", "Sonuç CSV", accept = c(".csv", "text/csv")),
        actionButton("import_postmatch", "Sonuçları hafızaya al", class = "btn-primary"),
        uiOutput("import_status")
      ),
      div(class = "panel", h3("Model sağlık kartı"), tableOutput("scorecard_table"))
    ),
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
    div(class = "lineup-head", div(h2(team$team), span(team$formation)), status_badge("Tahmin", "demo")),
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

