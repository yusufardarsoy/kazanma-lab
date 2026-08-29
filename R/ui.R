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
        status_badge("Süper Lig + ücretsiz web verisi", "live"),
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
          choiceNames = c("Maç merkezi", "Oran radarı", "Muhtemel 11", "AI Taktik & Heatmap", "Stil savaşı", "Süper Lig DNA", "Oyuncu radarları", "Model hafızası", "Literatür & Teori"),
          choiceValues = c("overview", "odds", "lineups", "agent_tactics", "styles", "teams", "players", "memory", "knowledge"),
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
          choices = super_lig_fixture_choices(db_path = config$db_path),
          options = list(placeholder = "Takım veya maç ara", maxOptions = 306)
        ),
        actionButton("run_analysis", "Tahmini kaydet", class = "btn-primary btn-block"),
        actionButton("sync_live", "Şimdi veri eşitle", class = "btn-secondary btn-block"),
        div(
          class = "source-note",
          strong("Anahtarsız temel akış"),
          span("Football-Data.co.uk arşiv sonuç ve oranlarını anahtarsız eşitler. Ücretsiz API-Football anahtarı güncel oran, fikstür, ilk 11 ve sakatlık verisini otomatik ekler.")
        ),
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
      div(
        class = "panel",
        div(class = "panel-kicker", "KESİN SKOR SIRALAMASI"),
        h3("En Olası Kesin Skor Tahminleri"),
        p("Modelin hesapladığı Poisson gol dağılımına göre en yüksek olasılıklı 8 skor."),
        plotOutput("top_scores_plot", height = 360),
        div(style = "margin-top: 14px;", tableOutput("top_scores_table"))
      ),
      div(
        class = "panel",
        div(class = "panel-kicker", "İLK YARI / MAÇ SONU"),
        h3("İY/MS Kombinasyon Olasılıkları"),
        p("İlk yarı temposu ve ikinci yarı gol beklentilerine göre 9 senaryonun ortak olasılıkları."),
        plotOutput("htft_plot", height = 360),
        div(style = "margin-top: 14px;", tableOutput("htft_table"))
      )
    ),
    div(
      class = "two-column",
      div(class = "panel", plotOutput("score_plot", height = 340)),
      div(class = "panel", plotOutput("outcome_plot", height = 340))
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
        h3("Bu tahmini nasıl okumalı?"),
        p("Olasılıklar takım gücü, taktik eşleşme ve tempo parametrelerinden türetilir. Biten maçlar hafızaya işlendikçe model skor dağılımlarını yeniden kalibre eder."),
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
    uiOutput("availability_status"),
    div(class = "panel", h3("Sakatlık ve cezalı listesi"), tableOutput("availability_table")),
    div(class = "panel caveat-panel", strong("Önemli:"), " Resmi ilk 11 genellikle başlama vuruşuna yakın gelir. O ana kadar ekrandaki değerler tahmindir; resmi veri geldiğinde durum “onaylı” olarak değişmelidir.")
  )
}

agent_tactics_ui <- function() {
  tagList(
    div(
      class = "section-heading",
      div(class = "eyebrow", "NVIDIA NIM LLAMA 3.2 VISION · SPATIAL HEATMAP RADAR"),
      h1("Yapay Zeka Taktik Ajanı & Oyuncu Isı Haritası"),
      p("ScraperFC ve Sofascore saha koordinat verileriyle oluşturulan 2D Gaussian ısı haritaları, oyuncuların koridor hakimiyetini ve NVIDIA Llama 3.2 Vision taktiksel scout analizini sunar.")
    ),
    div(
      class = "panel",
      div(class = "panel-kicker", "TAKTIK EŞLEŞME SEÇİCİ"),
      h3("Koridor ve Oyuncu Eşleşmesi"),
      div(
        class = "two-column",
        div(
          uiOutput("tactics_home_player_select")
        ),
        div(
          uiOutput("tactics_away_player_select")
        )
      ),
      div(
        style = "margin-top: 14px;",
        actionButton("generate_ai_scout", "NVIDIA Llama 3.2 ile AI Taktik Raporu Üret ⚡", class = "btn-primary btn-block")
      )
    ),
    div(
      class = "two-column",
      div(
        class = "panel",
        div(class = "panel-kicker", "EV SAHİBİ OYUNCUSU ISI HARİTASI"),
        uiOutput("home_player_heatmap_view"),
        div(style = "margin-top: 14px;", uiOutput("home_player_zone_bars"))
      ),
      div(
        class = "panel",
        div(class = "panel-kicker", "DEPLASMAN RAKİP KORİDOR ISI HARİTASI"),
        uiOutput("away_player_heatmap_view"),
        div(style = "margin-top: 14px;", uiOutput("away_player_zone_bars"))
      )
    ),
    div(
      class = "panel",
      div(class = "panel-kicker", "NVIDIA NIM SCOUT RAPORU"),
      h3("Yapay Zeka Taktiksel Çatışma & Boşluk Analizi"),
      uiOutput("nvidia_ai_scout_report_view")
    )
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
    div(
      class = "section-heading",
      div(class = "eyebrow", "LEARNING LOOP & POST-MATCH INTELLIGENCE"),
      h1("Model hafızası & Biten maçlar"),
      p("Biten maçların skorlarını, xG ve kart istatistiklerini izler; modelin bu sonuçlardan öğrenerek takımların hücum/savunma güçlerini nasıl güncellediğini gösterir.")
    ),
    uiOutput("memory_hero_cards"),
    div(
      class = "panel",
      div(class = "panel-kicker", "MODEL ÖĞRENME EVRİMİ"),
      h3("Biten maçlar sonrası güncellenen takım güçleri"),
      p("Gri nokta sezon başı öncülünü; renkli ok ve nokta ise biten maçların (xG, goller, kartlar) modele ağırlıklı etkisiyle oluşan güncel seviyeyi gösterir."),
      plotOutput("learning_evolution_plot", height = 480)
    ),
    div(
      class = "panel",
      div(class = "panel-kicker", "GÜNCEL TAKIM STİLLERİ VE FORM TABLOSU"),
      h3("Takım bazında öğrenilen hücum, savunma ve disiplin puanları"),
      tableOutput("team_learning_table")
    ),
    div(
      class = "panel",
      div(class = "panel-kicker", "TAMAMLANAN MAÇLAR KATALOĞU"),
      h3("Biten maçlar, gerçekleşen istatistikler ve tahmin durumu"),
      p("Maç sonu gerçek skorlar, xG ve kart verileri otomatik veya manuel olarak buraya işlenir."),
      tableOutput("finished_matches_table")
    ),
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
    div(class = "panel", div(class = "panel-kicker", "MODEL DEĞERLENDİRME"), h3("Skor & İY/MS Tahminleri — Gerçek Sonuç Karşılaştırması"), p("Dondurulmuş tahminlerin gerçekleşen kesin skor, Top-3 skor ve İY/MS ile doğruluk karşılaştırması."), tableOutput("comparison_table")),
    div(class = "panel", h3("Son analizler"), tableOutput("history_table")),
    div(
      class = "panel roadmap-panel",
      h3("Öğrenme döngüsü"),
      div(class = "roadmap-step", span("01"), div(strong("Tahmin anı"), p("Olasılıklar ve veri sürümü maç öncesinde dondurulur."))),
      div(class = "roadmap-step", span("02"), div(strong("Maç sonu"), p("Skor, xG, kart ve olay verileri hafızaya işlenir."))),
      div(class = "roadmap-step", span("03"), div(strong("Kalibrasyon"), p("Brier ve log loss ile model tahmini ölçülür."))),
      div(class = "roadmap-step", span("04"), div(strong("Dinamik Güncelleme"), p("Takım hücum/savunma güçleri ve oyun stilleri güncellenir.")))
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

knowledge_ui <- function() {
  tagList(
    div(
      class = "section-heading",
      div(class = "eyebrow", "MATHEMATICAL FOUNDATIONS & FOOTBALL PAPERS"),
      h1("Modelin Bilimsel & İstatistiksel Bilgi Tabanı"),
      p("Kazanma Lab, rastgele sayı üreten bir yapı değil; dünyanın önde gelen spor analitiği ve futbol modelleme makalelerinde kanıtlanmış matematiksel teoremleri temel alan deterministik bir tahmin motorudur.")
    ),
    div(
      class = "two-column",
      div(
        class = "panel",
        div(class = "panel-kicker", "TEMEL MAKALE 1 · DÜŞÜK SKOR KALİBRASYONU"),
        h3("Dixon & Coles (1997) Modeli"),
        p(strong("Makale:"), " Modelling Association Football Scores and Inefficiencies in the Football Betting Market (Journal of the Royal Statistical Society)."),
        div(
          class = "insight-item",
          div(class = "insight-dot"),
          p(strong("Düşük Skor Bağımlılığı (rho = -0.085):"), " Standart Poisson dağılımı 0-0, 1-1 ve 0/1 skorlarını eksik hesaplar. Dixon-Coles tau düzeltmesi ile bu skorların karşılıklı taktiksel kilitlenme olasılığı yükseltilir.")
        ),
        div(
          class = "insight-item",
          div(class = "insight-dot"),
          p(strong("Üstel Zaman Ağırlıklandırması (exp(-xi * dt)):"), " Son haftalarda oynanan maçların takım güçleri ve stilleri üzerindeki öğrenme etkisi, aylar önceki maçlara göre üstel olarak daha ağırdır.")
        )
      ),
      div(
        class = "panel",
        div(class = "panel-kicker", "TEMEL MAKALE 2 · İLK YARI / MAÇ SONU"),
        h3("Karlis & Ntzoufras (2003 / 2008) İki Aşamalı Dağılım"),
        p(strong("Makale:"), " Analysis of sports data by using bivariate Poisson models & Bayesian modelling of Half-Time/Full-Time outcomes."),
        div(
          class = "insight-item",
          div(class = "insight-dot"),
          p(strong("İlk Yarı / İkinci Yarı Dinamiği:"), " Futbol maçlarında gollerin ~%44'ü ilk yarıda, ~%56'sı ikinci yarıda (yorgunluk, taktiksel risk alma ve oyuncu değişiklikleri nedeniyle) atılır.")
        ),
        div(
          class = "insight-item",
          div(class = "insight-dot"),
          p(strong("9 Ortak İY/MS Olasılık Matrisi:"), " 0/1, 1/1, 0/0, 1/0 gibi senaryolar bağımsız değil; 1. yarı skoru ile 2. yarı gol beklentisinin ortak olasılık integrali olarak hesaplanır.")
        )
      )
    ),
    div(
      class = "two-column",
      div(
        class = "panel",
        div(class = "panel-kicker", "TEMEL MAKALE 3 · AKSİYON DEĞERLEMESİ VE TAKTİK"),
        h3("Decroos et al. (KDD 2019) & Berrar (2019)"),
        p(strong("Makale:"), " VAEP: Valuing Actions by Estimating Probabilities & Incorporating Game State."),
        div(
          class = "insight-item",
          div(class = "insight-dot"),
          p(strong("Taktiksel DNA Çakışması:"), " Takımın ön alan presi, rakibin geriden çıkarken top kaybı zaafıyla; geçiş hücumu hızı, rakibin geçiş savunması zaafıyla eşleştirilir.")
        ),
        div(
          class = "insight-item",
          div(class = "insight-dot"),
          p(strong("Oyun Durumu Etkisi (Game State):"), " Öne geçen takımların savunmaya çekilme ve kontra bekleme eğilimleri takım stil parametrelerine yansıtılır.")
        )
      ),
      div(
        class = "panel",
        div(class = "panel-kicker", "TEMEL MAKALE 4 · MODEL KALİBRASYONU & SAĞLIK"),
        h3("Brier (1950) & Wheatcroft (2020) Doğrulama"),
        p(strong("Makale:"), " Evaluating the performance of football score prediction models & Rank Probability Score."),
        div(
          class = "insight-item",
          div(class = "insight-dot"),
          p(strong("Çok Boyutlu Brier Skoru & Log-Loss:"), " Modelin başarısı sadece 'bildi/bilmedi' ile değil, gerçekleşen kesin skora atanan olasılığın güvenilirliği ve logaritmik kaybı üzerinden her maç kalibre edilir.")
        ),
        div(
          class = "insight-item",
          div(class = "insight-dot"),
          p(strong("Top-3 Skor Kapsama Oranı:"), " Futbolun doğal varyansı gereği en olası 3 skorun gerçekleşen maçı karşılama gücü ölçülür.")
        )
      )
    )
  )
}

