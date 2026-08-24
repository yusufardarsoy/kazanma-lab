ODDS_FIXTURE_ID <- "317800"
ODDS_SNAPSHOT_AT <- as.POSIXct("2026-08-24 12:00:00", tz = "Europe/Istanbul")

kocaeli_amed_odds <- function() {
  base <- tibble::tribble(
    ~market_id, ~market, ~selection_id, ~selection, ~odds, ~exhaustive, ~risk, ~reliability, ~active, ~note,
    "result", "Maç sonucu", "home", "Kocaelispor", 1.97, TRUE, "Orta", .82, TRUE, "90 dakika",
    "result", "Maç sonucu", "draw", "Beraberlik", 3.05, TRUE, "Orta", .82, TRUE, "90 dakika",
    "result", "Maç sonucu", "away", "Amed SK", 2.92, TRUE, "Orta", .82, TRUE, "90 dakika",
    "double_chance", "Çifte şans", "1X", "Kocaelispor veya beraberlik", 1.20, FALSE, "Düşük-orta", .82, TRUE, "Seçenekler birbiriyle örtüşür",
    "double_chance", "Çifte şans", "12", "Kocaelispor veya Amed SK", 1.18, FALSE, "Düşük-orta", .82, TRUE, "Seçenekler birbiriyle örtüşür",
    "double_chance", "Çifte şans", "X2", "Beraberlik veya Amed SK", 1.48, FALSE, "Düşük-orta", .82, TRUE, "Seçenekler birbiriyle örtüşür",
    "ou_1_5", "Toplam gol 1,5", "under", "1,5 alt", 2.82, TRUE, "Orta", .78, TRUE, "Poisson toplam gol modeli",
    "ou_1_5", "Toplam gol 1,5", "over", "1,5 üst", 1.22, TRUE, "Orta", .78, TRUE, "Poisson toplam gol modeli",
    "ou_2_5", "Toplam gol 2,5", "under", "2,5 alt", 1.53, TRUE, "Orta", .78, TRUE, "Poisson toplam gol modeli",
    "ou_2_5", "Toplam gol 2,5", "over", "2,5 üst", 1.90, TRUE, "Orta", .78, TRUE, "Poisson toplam gol modeli",
    "ou_3_5", "Toplam gol 3,5", "under", "3,5 alt", 1.12, TRUE, "Orta", .78, TRUE, "Poisson toplam gol modeli",
    "ou_3_5", "Toplam gol 3,5", "over", "3,5 üst", 3.49, TRUE, "Orta", .78, TRUE, "Poisson toplam gol modeli",
    "btts", "Karşılıklı gol", "yes", "Evet", 1.71, TRUE, "Orta", .76, TRUE, "İki takımın da gol bulması",
    "btts", "Karşılıklı gol", "no", "Hayır", 1.69, TRUE, "Orta", .76, TRUE, "İki takımın da gol bulmaması",
    "total_bucket", "Toplam gol aralığı", "0_1", "0-1 gol", 2.76, TRUE, "Orta-yüksek", .73, TRUE, "Toplam gol Poisson dağılımı",
    "total_bucket", "Toplam gol aralığı", "2_3", "2-3 gol", 1.77, TRUE, "Orta-yüksek", .73, TRUE, "Toplam gol Poisson dağılımı",
    "total_bucket", "Toplam gol aralığı", "4_5", "4-5 gol", 4.11, TRUE, "Orta-yüksek", .73, TRUE, "Toplam gol Poisson dağılımı",
    "total_bucket", "Toplam gol aralığı", "6_plus", "6+ gol", 19.80, TRUE, "Yüksek", .70, TRUE, "Toplam gol Poisson dağılımı",
    "first_half", "İlk yarı sonucu", "home", "Kocaelispor", 2.65, TRUE, "Orta-yüksek", .62, TRUE, "İlk yarı xG payı yaklaşık %45",
    "first_half", "İlk yarı sonucu", "draw", "Beraberlik", 1.90, TRUE, "Orta-yüksek", .62, TRUE, "İlk yarı xG payı yaklaşık %45",
    "first_half", "İlk yarı sonucu", "away", "Amed SK", 3.67, TRUE, "Orta-yüksek", .62, TRUE, "İlk yarı xG payı yaklaşık %45",
    "first_goal", "İlk gol", "home", "Kocaelispor", 1.67, TRUE, "Orta-yüksek", .64, TRUE, "Bağımsız gol süreçleri yaklaşımı",
    "first_goal", "İlk gol", "none", "Gol olmaz", 9.28, TRUE, "Yüksek", .64, TRUE, "Bağımsız gol süreçleri yaklaşımı",
    "first_goal", "İlk gol", "away", "Amed SK", 2.12, TRUE, "Orta-yüksek", .64, TRUE, "Bağımsız gol süreçleri yaklaşımı"
  )

  score_labels <- c(
    "1-0", "2-0", "2-1", "3-0", "3-1", "3-2", "4-0", "4-1", "4-2", "5-0", "5-1", "6-0",
    "0-0", "1-1", "2-2", "3-3",
    "0-1", "0-2", "1-2", "0-3", "1-3", "2-3", "0-4", "1-4", "2-4", "0-5", "1-5", "0-6", "Diğer"
  )
  score_odds <- c(
    6.00, 8.57, 7.91, 18.30, 16.85, 31.00, 53.00, 48.00, 88.00, 130.00, 130.00, 130.00,
    7.64, 5.30, 13.75, 81.00,
    7.62, 13.85, 10.05, 38.00, 27.50, 39.50, 130.00, 100.00, 130.00, 130.00, 130.00, 130.00, 51.00
  )
  exact <- tibble::tibble(
    market_id = "exact_score",
    market = "Doğru skor",
    selection_id = ifelse(score_labels == "Diğer", "other", paste0("score_", gsub("-", "_", score_labels))),
    selection = score_labels,
    odds = score_odds,
    exhaustive = TRUE,
    risk = "Çok yüksek",
    reliability = .60,
    active = TRUE,
    note = "Küçük olasılık; Diğer seçeneği kalan skorları kapsar"
  )

  scorers <- tibble::tribble(
    ~selection_id, ~selection, ~odds, ~active, ~note,
    "Bruno Petkovic", "Bruno Petković", 2.22, FALSE, "Doğrulanmış eksik; oran bayat kabul edildi",
    "Daniel Agyei", "Daniel Agyei", 2.70, TRUE, "Muhtemel 11",
    "Metehan Altunbas", "Metehan Altunbaş", 2.70, TRUE, "Muhtemel 11",
    "Mbaye Diagne", "Mbaye Diagne", 2.86, TRUE, "Muhtemel 11",
    "Makana Baku", "Makana Baku", 2.90, TRUE, "Muhtemel 11",
    "Gift Orban", "Gift Orban", 2.97, TRUE, "Muhtemel 11",
    "Dia Saba", "Dia Saba", 3.44, TRUE, "Muhtemel 11",
    "Yira Sor", "Yira Sor", 4.54, TRUE, "Muhtemel 11",
    "Ermal Krasniqi", "Ermal Krasniqi", 4.60, TRUE, "Muhtemel 11"
  ) |>
    dplyr::mutate(
      market_id = "scorer",
      market = "Gol atar",
      exhaustive = FALSE,
      risk = "Yüksek",
      reliability = .58
    ) |>
    dplyr::select(names(base))

  dplyr::bind_rows(base, exact, scorers) |>
    dplyr::mutate(
      fixture_id = ODDS_FIXTURE_ID,
      snapshot_at = ODDS_SNAPSHOT_AT,
      source = "Kullanıcının gönderdiği İddaa oran görüntüsü"
    )
}

model_market_probabilities <- function(prediction) {
  hx <- unname(prediction$expected_goals[["home"]])
  ax <- unname(prediction$expected_goals[["away"]])
  lambda <- hx + ax
  full_matrix <- score_probability_matrix(hx, ax, max_goals = 12L)
  outcomes <- outcome_probabilities(full_matrix)
  half_matrix <- score_probability_matrix(hx * .45, ax * .45, max_goals = 8L)
  half_outcomes <- outcome_probabilities(half_matrix)
  no_goal <- exp(-lambda)

  core <- tibble::tribble(
    ~market_id, ~selection_id, ~model_probability,
    "result", "home", outcomes[["home"]],
    "result", "draw", outcomes[["draw"]],
    "result", "away", outcomes[["away"]],
    "double_chance", "1X", outcomes[["home"]] + outcomes[["draw"]],
    "double_chance", "12", outcomes[["home"]] + outcomes[["away"]],
    "double_chance", "X2", outcomes[["draw"]] + outcomes[["away"]],
    "ou_1_5", "under", stats::ppois(1, lambda),
    "ou_1_5", "over", 1 - stats::ppois(1, lambda),
    "ou_2_5", "under", stats::ppois(2, lambda),
    "ou_2_5", "over", 1 - stats::ppois(2, lambda),
    "ou_3_5", "under", stats::ppois(3, lambda),
    "ou_3_5", "over", 1 - stats::ppois(3, lambda),
    "btts", "yes", (1 - exp(-hx)) * (1 - exp(-ax)),
    "btts", "no", 1 - (1 - exp(-hx)) * (1 - exp(-ax)),
    "total_bucket", "0_1", stats::ppois(1, lambda),
    "total_bucket", "2_3", stats::ppois(3, lambda) - stats::ppois(1, lambda),
    "total_bucket", "4_5", stats::ppois(5, lambda) - stats::ppois(3, lambda),
    "total_bucket", "6_plus", 1 - stats::ppois(5, lambda),
    "first_half", "home", half_outcomes[["home"]],
    "first_half", "draw", half_outcomes[["draw"]],
    "first_half", "away", half_outcomes[["away"]],
    "first_goal", "home", (1 - no_goal) * hx / lambda,
    "first_goal", "none", no_goal,
    "first_goal", "away", (1 - no_goal) * ax / lambda
  )

  score_rows <- kocaeli_amed_odds() |>
    dplyr::filter(market_id == "exact_score", selection_id != "other") |>
    dplyr::mutate(
      home_goals = as.integer(sub("score_([0-9]+)_([0-9]+)", "\\1", selection_id)),
      away_goals = as.integer(sub("score_([0-9]+)_([0-9]+)", "\\2", selection_id)),
      model_probability = stats::dpois(home_goals, hx) * stats::dpois(away_goals, ax)
    ) |>
    dplyr::select(market_id, selection_id, model_probability)
  exact <- dplyr::bind_rows(
    score_rows,
    tibble::tibble(market_id = "exact_score", selection_id = "other", model_probability = 1 - sum(score_rows$model_probability))
  )

  player_name_map <- c(
    "Daniel Agyei" = "Daniel Agyei", "Metehan Altunbas" = "Metehan Altunbaş",
    "Mbaye Diagne" = "Mbaye Diagne", "Makana Baku" = "Makana Baku",
    "Gift Orban" = "Gift Orban", "Dia Saba" = "Dia Saba",
    "Yira Sor" = "Yira Sor", "Ermal Krasniqi" = "Ermal Krasniqi"
  )
  players <- prediction$player_markets |>
    dplyr::filter(player %in% unname(player_name_map)) |>
    dplyr::transmute(
      market_id = "scorer",
      selection_id = names(player_name_map)[match(player, player_name_map)],
      model_probability = scorer_probability
    )
  dplyr::bind_rows(core, exact, players)
}

validate_odds_snapshot <- function(odds = kocaeli_amed_odds()) {
  required <- c("fixture_id", "market_id", "selection_id", "odds", "exhaustive", "active")
  if (!all(required %in% names(odds))) stop("Oran görüntüsü gerekli alanları taşımıyor.")
  if (any(!is.finite(odds$odds)) || any(odds$odds <= 1)) stop("Tüm ondalık oranlar 1'den büyük olmalı.")
  if (anyDuplicated(paste(odds$market_id, odds$selection_id))) stop("Aynı market seçeneği birden çok kez girilmiş.")
  if (any(odds$fixture_id != ODDS_FIXTURE_ID)) stop("Oran görüntüsü başka bir maça karışmış.")
  invisible(TRUE)
}

compare_odds <- function(prediction, odds = kocaeli_amed_odds()) {
  if (!identical(as.character(prediction$fixture$fixture_id[[1]]), ODDS_FIXTURE_ID)) return(tibble::tibble())
  validate_odds_snapshot(odds)
  probs <- model_market_probabilities(prediction)
  odds |>
    dplyr::left_join(probs, by = c("market_id", "selection_id")) |>
    dplyr::mutate(implied_probability = 1 / odds) |>
    dplyr::group_by(market_id) |>
    dplyr::mutate(
      market_margin = if (dplyr::first(exhaustive)) sum(implied_probability) - 1 else NA_real_,
      market_probability = if (dplyr::first(exhaustive)) implied_probability / sum(implied_probability) else implied_probability
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      supported = !is.na(model_probability),
      edge = model_probability - market_probability,
      expected_return = model_probability * odds - 1,
      decision_score = edge * sqrt(pmax(model_probability, 0)) * reliability * dplyr::case_when(
        risk == "Çok yüksek" ~ .42,
        risk == "Yüksek" ~ .58,
        risk == "Orta-yüksek" ~ .72,
        TRUE ~ 1
      ),
      signal = dplyr::case_when(
        !active ~ "Kadro dışı / bayat oran",
        !supported ~ "Model yok",
        reliability >= .70 & edge >= .04 & expected_return >= .08 ~ "Modelde pozitif fark",
        edge >= .02 & expected_return >= .04 ~ "Sınırda — izle",
        TRUE ~ "Pas"
      ),
      reference_label = dplyr::if_else(exhaustive, "Marjsız piyasa", "Başabaş")
    ) |>
    dplyr::arrange(dplyr::desc(active), dplyr::desc(decision_score))
}

top_odds_options <- function(comparison, n = 8L) {
  if (nrow(comparison) == 0) return(comparison)
  comparison |>
    dplyr::filter(active, supported, market_id != "exact_score") |>
    dplyr::arrange(dplyr::desc(signal == "Modelde pozitif fark"), dplyr::desc(decision_score)) |>
    dplyr::slice_head(n = n)
}

odds_snapshot_summary <- function(comparison) {
  if (nrow(comparison) == 0) return(NULL)
  result <- comparison |> dplyr::filter(market_id == "result")
  list(
    market_favorite = result$selection[[which.min(result$odds)]],
    model_favorite = result$selection[[which.max(result$model_probability)]],
    bookmaker_margin = unique(result$market_margin)[[1]],
    positive_count = sum(comparison$signal == "Modelde pozitif fark", na.rm = TRUE),
    stale_count = sum(!comparison$active)
  )
}
